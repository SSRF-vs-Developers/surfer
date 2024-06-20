package de.tubs.cs.ias.surfer

import de.halcony.argparse.{Parser, ParsingException}
import de.tubs.cs.ias.cpg.dotFileExporter.conf.ExportConfig
import de.tubs.cs.ias.cpg.slicing.algorithms.util.CpgTraveler
import de.tubs.cs.ias.surfer.reversing.ReversingRuleDeployer
import io.shiftleft.codepropertygraph.generated.Cpg
import spray.json.DefaultJsonProtocol._
import spray.json._
import wvlet.log.LogSupport

import java.io.{File, FileWriter}
import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

object Surfer extends LogSupport {

  val parser: Parser =
    Parser("surfer", "finding and explaining your chill SSRF vulnerabilities")
      .addOptional("export",
                   "e",
                   "export",
                   Some("export.json"),
                   "path to the export configuration")
      .addFlag(
        "debug",
        "d",
        "debug",
        "if set only a single slice is processed at a time and more context information is given")
      .addPositional("outfile", "the file where the output goes")
      .addPositional("cpg", "the cpg to be analyzed")

  var cpg: Option[Cpg] = None

  private def generateInvolvedBytecodeReport(
      candidate: SSRFCandidate): JsObject = {
    try {
      JsObject(
        DataFlowBytecode
          .getInvolvedBytecodesTo(candidate, "FETCH.*global.*".r, "RECV.*".r)
          .map {
            case (key, value) => key.getRepresentation -> JsNumber(value)
          })
    } catch {
      case thr: Throwable =>
        var counter = -1
        JsObject(
          "message" -> JsString(thr.getMessage),
          "stacktrace" -> JsObject(
            thr.getStackTrace.map { entry =>
              counter += 1
              counter.toString -> JsString(entry.toString)
            }.toMap
          )
        )
    }
  }

  private def generateReport(candidates: List[JsValue],
                             errors: List[Throwable],
                             no_sinks: Int,
                             no_sources: Int): JsValue = {
    JsObject(
      "candidates" -> JsObject {
        candidates.indices.map { index =>
          index.toString -> candidates(index)
        }.toMap
      },
      "errors" -> JsObject(
        errors.indices.map { index =>
          var counter = -1
          index.toString ->
            JsObject(
              "error" -> JsString(errors(index).getMessage),
              "stacktrace" -> JsObject(
                errors(index).getStackTrace.map { elem =>
                  counter = counter + 1
                  counter.toString -> JsString(elem.toString)
                }.toMap
              )
            )
        }.toMap
      ),
      "defaultRuleUsage" -> ReversingRuleDeployer.getDefaultRuleUsage.toJson,
      "noSinks" -> no_sinks.toJson,
      "noSources" -> no_sources.toJson
    )
  }

  private def generateErrorReport(errors: List[Throwable]): JsValue = {
    JsObject(
      "errors" -> JsObject(
        errors.indices.map { index =>
          index.toString ->
            JsObject(
              "error" -> JsString(
                Option(errors(index).getMessage).getOrElse("null")),
              "stacktrace" -> JsArray(
                Option(errors(index).getStackTrace) match {
                  case Some(st) =>
                    st.map { elem =>
                      JsString(elem.toString)
                    }.toVector
                  case None => List().toVector
                }
              )
            )
        }.toMap
      )
    )
  }

  private def processCommandLineArguments(
      args: Array[String]): (String, String, String, Boolean) = {
    val pargs = parser.parse(args)
    val reportFileBase = pargs.getValue[String]("outfile")
    val cpgPath = pargs.getValue[String]("cpg")
    val debug = pargs.getValue[Boolean]("debug")
    val exportConfigFile = pargs.getValue[String]("export")
    (reportFileBase, cpgPath, exportConfigFile, debug)
  }

  private def aggregateCandiatesAndErrors(
      overall: List[Either[SSRFCandidate, Throwable]])
    : (List[SSRFCandidate], List[Throwable]) = {
    overall.foldRight((List[SSRFCandidate](), List[Throwable]())) {
      case (either, (results, errors)) =>
        either match {
          case Left(candidate) => (candidate :: results, errors)
          case Right(error)    => (results, error :: errors)
        }
    }
  }

  private def reverseSRRFCandidate(candidate: SSRFCandidate)(
      implicit cpg: Cpg): JsValue = {
    info(s"generating reversing report on $candidate")
    JsObject(
      "sources" -> JsArray(
        candidate.getSources
          .map(call => JsString(call.code))
          .toVector),
      "sink" -> JsString(candidate.getSink.name),
      "involvedBytecodes" -> generateInvolvedBytecodeReport(candidate),
      "reversedString" -> JsArray(
        candidate.getSinkStrings.map(JsString(_)).toVector),
      "sources" -> JsArray(
        candidate.getSources
          .map(_.getCpgCall)
          .map(source => {
            JsArray(
              Vector(
                JsString(CpgTraveler.getFile(source).get().name),
                JsString(CpgTraveler.getLineNumber(source).toString)
              ))
          })
          .toVector),
      "sink" -> JsArray(
        Vector(
          JsString(
            CpgTraveler.getFile(candidate.getSink.getCpgCall).get().name),
          JsString(
            CpgTraveler.getLineNumber(candidate.getSink.getCpgCall).toString)
        )),
      "sinkName" -> candidate.getSink.name.toJson
    )
  }

  /** function to take the CPG and perform the actual analysis on
    *
    * @param cpgPath the path to the cpg
    * @param reportOutBase the base file to write the report(s) to
    * @param debug flag whether to go single threaded debug
    */
  private def scourCpgForSSRF(
      cpgPath: String,
      reportOutBase: String,
      debug: Boolean)(implicit exportConfig: ExportConfig): Unit = {
    val reportFile = new FileWriter(new File(reportOutBase))
    try {
      implicit val cpg: Cpg = Cpg.withStorage(cpgPath)
      info(s"working with $cpgPath")
      val (overall, noSinks, noSources) =
        SSRFDetector.getCandidates(debug = debug)
      info(
        s"slicing done - encountered $noSinks times no sink and $noSources no sources")
      val (result, error) = aggregateCandiatesAndErrors(overall)
      info(
        s"we encountered ${error.length} errors and extracted ${result.length} candidates")
      var counter = 0
      result.foreach { candidate =>
        val dotFile =
          new FileWriter(new File(s"${reportOutBase}_$counter.dot"))
        counter = counter + 1
        try {
          dotFile.write(candidate.getSlice.toDot)
        } finally {
          dotFile.flush()
          dotFile.close()
        }
      }
      info("generating reports ...")
      val reports = if (debug) {
        result.map { candidate =>
          info(s"reversing $candidate")
          reverseSRRFCandidate(candidate)
        }
      } else {
        val future = Future.sequence {
          result.map { candidate =>
            Future {
              reverseSRRFCandidate(candidate)
            }
          }
        }
        Await.result(future, Inf)
      }
      reportFile.write(
        generateReport(reports,
                       error,
                       no_sinks = noSinks,
                       no_sources = noSources).prettyPrint)
    } catch {
      case e: Throwable =>
        error(e.toString)
        reportFile.write(generateErrorReport(List(e)).prettyPrint)
    } finally {
      reportFile.flush()
      reportFile.close()
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val (reportOutBase, cpgPath, exportConfigFile, debug) =
        processCommandLineArguments(args)
      implicit val exportConfig: ExportConfig =
        ExportConfig.apply(new File(exportConfigFile))
      if (!Files.exists(Paths.get(cpgPath))) {
        throw new RuntimeException(
          s"Passed CPG file >>$cpgPath<< doesn't exist!")
      }
      scourCpgForSSRF(cpgPath, reportOutBase, debug)(exportConfig)
    } catch {
      case _: ParsingException =>
      case x: Throwable =>
        error(x.getMessage)
        val reportOutBase = parser.parse(args).getValue[String]("outfile")
        val report = generateErrorReport(List(x)).prettyPrint
        val reportFile = new FileWriter(new File(reportOutBase))
        reportFile.write(report)
        reportFile.flush()
        reportFile.close()
    }

  }

}
