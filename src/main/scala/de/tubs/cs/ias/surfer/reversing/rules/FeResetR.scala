package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedIterator, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._

object FeResetR extends BytecodeReversingRule {

  override val BYTECODE_NAME: String = "FE_RESET_R"

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {

    val array :: integer :: Nil =
      current.getCpgCall.astChildren.sortBy(_.order).toList
    assert(integer.isInstanceOf[Literal])
    array match {
      case ident: Identifier =>
        val reversedArray: List[ReversedBlob] =
          current
            .inE(SliceEdgeTypes.DATA_DEPENDENCE)
            .filter(_.getAttribute("var").contains(ident.name))
            .map(_.out())
            .flatMap(node => ReversingRuleDeployer.deploy(node, context))
        reversedArray.map(elem => ReversedIterator(elem))
      case _ =>
        List(ReversedUnknown(AnalysisSteps.SURFER, "_NO_ARRAY_FOR_ITERATOR"))
    }

  }
}
