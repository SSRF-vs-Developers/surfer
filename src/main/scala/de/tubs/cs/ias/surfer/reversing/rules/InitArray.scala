package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedArrayElement, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

object InitArray extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "INIT_ARRAY"

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val (index, value) =
      current.getCpgCall.astChildren.toList.sortBy(_.order) match {
        case _ :: _ :: value :: index :: Nil => (index, value)
        case _ :: value :: index :: Nil      => (index, value)
        case _ =>
          List(
            ReversedUnknown(AnalysisSteps.SURFER,
                            "INIT_ARRAY_INDEX_VALUE_MATCH"))
      }
    //println(value.asInstanceOf[CfgNode].code)
    val indexi = index match {
      case literal: Literal =>
        List(ReversedLiteral(literal.code))
      case identifier: Identifier =>
        current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.attributes("var") == identifier.name)
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
      case weird =>
        List(
          ReversedUnknown(AnalysisSteps.SURFER,
                          s"BAD_ARRAY_INDEX_${weird.getClass.getName}"))
    }
    val values = value match {
      case literal: Literal =>
        List(ReversedLiteral(literal.code))
      case identifier: Identifier =>
        val ret = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.attributes("var") == identifier.name)
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
        if (ret.nonEmpty) {
          ret
        } else {
          List(
            ReversedUnknown(AnalysisSteps.SURFER,
                            s"BAD_ARRAY_VALUE_UNKNOWN_CV_${identifier.name}"))
        }
      case weird =>
        List(
          ReversedUnknown(AnalysisSteps.SURFER,
                          s"BAD_ARRAY_VALUE_${weird.getClass.getName}"))
    }
    indexi.flatMap { index =>
      values.map { value =>
        ReversedArrayElement(index, value, None)
      }
    }
  }

}
