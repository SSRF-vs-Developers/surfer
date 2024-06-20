package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedFCall, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.toCallMethods
import wvlet.log.LogSupport

/** reversing rule STRLEN
  *
  * @author Malte Wessels
  *
  */
object Strlen extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "STRLEN"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val nodes = current.in(SliceEdgeTypes.DATA_DEPENDENCE)
    if (nodes.nonEmpty) {
      nodes
        .flatMap(ReversingRuleDeployer.deploy(_, context))
        .map(x => ReversedFCall("strlen", Map("1" -> x), None))
    } else {
      current.getCpgCall.argument.l match {
        case (l: Literal) :: Nil =>
          List(ReversedLiteral(l.code.length.toString))
        case _ =>
          List(
            ReversedUnknown(AnalysisSteps.SURFER,
                            "SRLEN_UNEXEPECTED_ARGUMENT_TYPE"))
      }
    }
  }
}
