package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedArrayElement, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

object AssignDim extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ASSIGN_DIM"

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val array :: index :: Nil = current.getCpgCall.astChildren.toList
    array match {
      case ident: Identifier =>
        val arrays = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.attributes("var") == ident.name)
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
        val indexi = (index match {
          case ident: Identifier =>
            println(ident.name)
            current
              .inE(SliceEdgeTypes.DATA_DEPENDENCE)
              .filter(_.attributes("var") == ident.name)
              .map(_.out())
              .flatMap(node => ReversingRuleDeployer.deploy(node, context))
          case lit: Literal => List(ReversedLiteral(lit.code))
        })
        val values = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.attributes("var") == "<OP_DATA>")
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
        val adaptedArrays =
          if (arrays.nonEmpty) arrays
          else List(ReversedUnknown(AnalysisSteps.SURFER, "ARRAY_UNKNOWN"))
        val adaptedIndexi =
          if (indexi.nonEmpty) indexi
          else
            List(ReversedUnknown(AnalysisSteps.SURFER, "ARRAY_INDEX_UNKNOWN"))
        val adaptedValues =
          if (values.nonEmpty) values
          else
            List(ReversedUnknown(AnalysisSteps.SURFER, "ARRAY_VALUE_UNKONWN"))
        adaptedArrays.flatMap { array =>
          adaptedIndexi.flatMap { index =>
            adaptedValues.map { value =>
              ReversedArrayElement(index,
                                   value,
                                   if (array.isInstanceOf[ReversedUnknown]) None
                                   else Some(array))
            }
          }
        }
      case _ =>
        List(ReversedUnknown(AnalysisSteps.SURFER, "ARRAY_NOT_IDENTIFIER"))
    }
  }
}
