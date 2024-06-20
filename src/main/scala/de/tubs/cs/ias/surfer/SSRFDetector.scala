package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.cpg.slicing.representation.ProgramSlice
import de.tubs.cs.ias.surfer.sinks.SinkDefinition
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._
import overflowdb.Edge
import wvlet.log.LogSupport

import java.util.concurrent.ExecutionException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

object SSRFDetector extends LogSupport {

  private case class DetectedSinkParameter(definition: SinkDefinition,
                                           call: Call,
                                           edge: Option[Edge])

  private def sinkToSliceNodes(sinkDefinitions: Set[SinkDefinition])(
      implicit cpg: Cpg): List[DetectedSinkParameter] = {
    cpg.call
      .map(call => (call, sinkDefinitions.find(_.matches(call)))) // try to get a sink def. for all calls
      .collect { case x if x._2.nonEmpty => (x._1, x._2.get) } // discard those where we couldn't find one
      .map { x =>
        (x._1, x._2.getEdgeForSink(x._1), x._2)
      }
      .map {
        case (call, edgeOption, definition) =>
          DetectedSinkParameter(definition, call, edgeOption)
      }
      .toList
  }

  def getCandidates(debug: Boolean,
                    beAnnoying: Boolean = false,
                    context: String = "")(
      implicit cpg: Cpg): (List[Either[SSRFCandidate, Throwable]], Int, Int) = {
    Surfer.cpg = Some(cpg)
    val sinks = sinkToSliceNodes(SinkDefinition.getDefinitions)
    val sources: Set[Call] = CallFilter.getGlobalReads.toSet
    info(s"we have ${sinks.size} sinks and ${sources.size} sources")
    if (sinks.nonEmpty && sources.nonEmpty) {
      if (debug) {
        info("starting debug processing")
        (sinks.map {
          case DetectedSinkParameter(definition, call, _) =>
            try {
              info(s"slicing on $definition with $call")
              val slice: ProgramSlice = ProgramSlice.slice(call)
              info("slice created")
              // DEBUG CPG
              //implicit val config: ExportConfig = ExportConfig.apply(new File("export.json"))
              //info("displaying slice")
              //slice.showGraph()
              //info("displayed slice")
              Left(new SSRFCandidate(slice, definition).fullReversing())
            } catch {
              case e: Throwable =>
                Right(e)
            }
        }, sinks.size, sources.size)
      } else {
        info("starting parallelized processing")
        val future = Future.sequence {
          sinks.map {
            case DetectedSinkParameter(definition, call, _) =>
              Future {
                try {
                  val slice: ProgramSlice = ProgramSlice.slice(call)
                  Left(
                    new SSRFCandidate(slice, definition)
                      .fullReversing(beAnnoying, context))
                } catch {
                  case boxed: ExecutionException => Right(boxed.getCause)
                  case e: Throwable              => Right(e)
                }
              }
          }
        }
        (Await.result(future, Inf), sinks.size, sources.size)
      }
    } else {
      (List(), sinks.size, sources.size)
    }
  }

}
