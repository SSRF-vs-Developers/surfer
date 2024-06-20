package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{Not, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

/** reversing rule BOOL_NOT
  *
  * @author Malte Wessels
  *
  */
object BoolNot extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "BOOL_NOT"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val nodes = current.in(SliceEdgeTypes.DATA_DEPENDENCE)
    if (nodes.nonEmpty) {
      nodes
        .flatMap { node =>
          ReversingRuleDeployer.deploy(node, context)
        }
        .map(Not)
    } else {
      current.getCpgCall.get.astChildren.order(1).nextOption() match {
        case Some(x: Literal) =>
          List(Not(ReversedLiteral(x.code)))
        case None =>
          List(
            ReversedUnknown(AnalysisSteps.SURFER,
                            "BOOL_NOT_NEXT_NONE_" + current.code))
        case _ =>
          warn(
            s"really weird assign without outgoing edge ${current.getCpgCall.get.code}")
          // this could also be a CPG issue but we will assume correctness for now (until proven otherwise)
          List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN_BOOL_NOT_ASSIGN"))
      }
    }
  }
}
