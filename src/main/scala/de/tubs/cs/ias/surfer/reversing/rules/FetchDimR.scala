package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedArrayAccess, ReversedLiteral}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Identifier, Literal}
import io.shiftleft.semanticcpg.language._

object FetchDimR extends BytecodeReversingRule {

  override val BYTECODE_NAME: String = "FETCH_DIM_R"

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val array :: index :: Nil = current.getCpgCall.astChildren
      .sortBy(_.order)
      .toList
      .map(_.asInstanceOf[CfgNode])
    val arrayCandidates = array match {
      case arrayIdent: Identifier =>
        current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.getAttribute("var").contains(arrayIdent.name))
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
    }
    val indexCandidates = index match {
      case indexIdent: Identifier =>
        current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.getAttribute("var").contains(indexIdent.name))
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
      case indexLit: Literal =>
        List(ReversedLiteral(indexLit.code))
    }
    // go over all reversed array candidates
    arrayCandidates.flatMap { ra =>
      indexCandidates.map { ic =>
        ReversedArrayAccess(ra, ic)
      }
    }
  }

}
