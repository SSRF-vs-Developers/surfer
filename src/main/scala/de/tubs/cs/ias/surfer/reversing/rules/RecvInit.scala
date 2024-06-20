package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedCFG
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

import scala.jdk.CollectionConverters.IteratorHasAsScala

/** Reversing Rule for RECV
  *
  * @author Malte Wessels
  *
  */
object RecvInit extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "RECV_INIT"

  private def extractDefaultValue(current: SliceNode): List[ReversedBlob] = {
    current.getCpgCall.astChildren.order(1).headOption match {
      case Some(literal: Literal) =>
        List(ReversedLiteral(literal.code))
      case None =>
        warn("no literal in RECV_INIT")
        List(ReversedUnknown(AnalysisSteps.CPG, "NO_LITERAL_RECV_INIT")) //this must not happen and points to a CPG issue
      case x =>
        throw new RuntimeException(
          s"unclear how to handle default argument of $current : $x")
    }
  }

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    // if we have a call stack
    if (context.callStack.nonEmpty) {
      // get the current function call on stack
      val lastFunctionCall = context.callStack.head
      // get its corresponding send_val previous intra function nodes
      val sendVals = lastFunctionCall.in(SliceEdgeTypes.DATA_DEPENDENCE)
      // find the send val that connects to the current RECV_INIT
      sendVals.filter(_.out(SliceEdgeTypes.SEND_VAR).contains(current)) match {
        case Nil => // if there is no such intersection we resort to the default argument provided
          // (note that we do not have to pop explicitly, the DFS will take care of that)
          extractDefaultValue(current)
        case multiple => // if there is a match we continue from there and pop the top of the call stack
          multiple.flatMap(node =>
            ReversingRuleDeployer.deploy(node, context.popStack()))
      }
    } else {
      val callPoints: List[Call] = current.getCpgCall.getParentMethod.get
        .in(EdgeTypes.CALL)
        .asScala
        .toList
        .map(_.asInstanceOf[Call])
      val sendLessCallPoints: List[Call] =
        callPoints.filterNot(
          // check if call has no argument connection to a send node - if so it is a send less call
          _.out(EdgeTypes.ARGUMENT).asScala
            .filter(_.isInstanceOf[Call])
            .exists(_.asInstanceOf[Call].name.contains("SEND"))
        )
      val targets: List[SliceNode] = current.in(SliceEdgeTypes.SEND_VAR)
      targets match {
        case Nil =>
          extractDefaultValue(current)
        case multiple =>
          val ret = multiple.flatMap(sendNode =>
            ReversingRuleDeployer.deploy(sendNode, context))
          if (sendLessCallPoints.nonEmpty)
            ret ++ extractDefaultValue(current)
          else
            ret
      }
    }
  }
}
