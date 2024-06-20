package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{Assignment, ReversedArrayElement, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._
import spray.json.DefaultJsonProtocol._
import spray.json._
import wvlet.log.LogSupport

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.util.Try

/** reversing rule ASSIGN
  *
  * @author Simon Koch
  *
  */
object Assign extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ASSIGN"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val nodes = current.in(SliceEdgeTypes.DATA_DEPENDENCE)
    debug(current.code)
    if (nodes.nonEmpty) {
      debug("reversing assign with children")
      nodes.flatMap { node =>
        ReversingRuleDeployer.deploy(node, context).map { result =>
          Assignment(result)
        }
      }
    } else {
      debug("reversing assign without children")
      val thing = current.getCpgCall.get.astChildren.order(1).next()
      thing match {
        // base64 regex modified after https://stackoverflow.com/a/63758783/
        case x: Literal
            if x.code.matches(
              "^array\\((?=(.{4})*\\)$)[A-Za-z0-9+/]*={0,2}\\)$") =>
          val base64str = x.code.stripPrefix("array(").stripSuffix(")")
          val jsonstr = Try(
            new String(Base64.getDecoder.decode(base64str),
                       StandardCharsets.UTF_8))
          if (jsonstr.isFailure)
            return List(
              ReversedUnknown(AnalysisSteps.CPG, "ASSIGN_ARRAY_B64_ERROR"))
          val array = Try(jsonstr.get.parseJson.convertTo[Map[String, JsValue]])
          if (array.isFailure)
            return List(
              ReversedUnknown(AnalysisSteps.SURFER,
                              "ASSIGN_ARRAY_JSON_PARSE_FAILED"))
          val array_reversed: Map[ReversedBlob, ReversedBlob] =
            array.get.map(x =>
              (ReversedLiteral(x._1), x._2 match {
                case JsObject(fields) =>
                  ReversedUnknown(AnalysisSteps.SURFER, "ASSIGN_ARRAY_JSOBJECT")
                case JsArray(elements) =>
                  ReversedUnknown(AnalysisSteps.SURFER, "ASSIGN_ARRAY_JSARRAY")
                case JsString(value)    => ReversedLiteral(value)
                case JsNumber(value)    => ReversedLiteral(value.toString())
                case boolean: JsBoolean => ReversedLiteral(boolean.toString())
                case JsNull             => ReversedLiteral("null")
              }))
          ReversedArrayElement.createFromMap(array_reversed) match {
            case Some(value) => List(value)
            case None =>
              debug("empty const array detected")
              List()
          }

        case x: Literal => List(Assignment(ReversedLiteral(x.code)))
        case _ =>
          warn(
            s"really weird assign without incoming edge ${current.getCpgCall.get.code}")
          List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN_ASSIGN"))
      }
    }
  }
}
