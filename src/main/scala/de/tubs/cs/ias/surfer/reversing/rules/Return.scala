package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

/** Reversing rule for RETURN
  *
  * @author Malte Wessels
  *
  */
object Return extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "RETURN"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val arg_node = current.getCpgCall.argument.filter(_.order == 0).l.head
    arg_node match {
      case l: Literal => List(ReversedLiteral(l.code))
      case _: Identifier =>
        current
          .in(SliceEdgeTypes.DATA_DEPENDENCE)
          .flatMap { edge =>
            ReversingRuleDeployer.deploy(edge, context)
          }
      case x =>
        warn(s"unable to handle unexpected argument child of RETURN $x")
        List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN_CHILD_RETURN"))
    }
  }
}
