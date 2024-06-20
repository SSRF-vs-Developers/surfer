package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedArrayElement, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedAST
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

import java.util.UUID

object AddArrayElement extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ADD_ARRAY_ELEMENT"

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val in = current.in(SliceEdgeTypes.DATA_DEPENDENCE)
    val previousArrayOperation = in
      .filter(elem =>
        Set("INIT_ARRAY", "ADD_ARRAY_ELEMENT").contains(elem.getCpgCall.name))
      .flatMap { elem =>
        ReversingRuleDeployer.deploy(elem, context)
      }
    val method = current.getCpgCall.getParentMethod.get
    val dump: String = s"/home/simon/tmp/${UUID.randomUUID().toString}.dot"
    if (previousArrayOperation.isEmpty) {
      // DEBUG CPG
      //implicit val config: ExportConfig =
      //  ExportConfig.apply(new File("export.json"))
      //info("dumping slice")
      //info(previousArrayOperation)
      info(s"for an ADD_ARRAY_ELEMENT there must be a preceding array operation but only found ${in
        .map(_.name)
        .mkString("(", ",", ")")} in file ${method.filename}:${method.name} dump ${dump} command ${current}")
      //current.getSlice.showGraph()
      //Files.writeString(
      //  Paths.get(dump),
      //  current.getSlice.toDot
      //)
      //info("displayed slice")
    }
    assert(
      previousArrayOperation.nonEmpty,
      s"for an ADD_ARRAY_ELEMENT there must be a preceding array operation but only found ${in
        .map(_.name)
        .mkString("(", ",", ")")} in file ${method.filename}:${method.name} dump ${dump}"
    )
    val indexi = current.getCpgCall.astChildren.order(1).next() match {
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
    val values = current.getCpgCall.astChildren.order(0).next() match {
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
                          s"BAD_ARRAY_VALUE_${weird.getClass.getName}"))
    }
    previousArrayOperation.flatMap { previousOp =>
      indexi.flatMap { index =>
        values.map { value =>
          ReversedArrayElement(
            index,
            value,
            Some(previousOp.asInstanceOf[ReversedArrayElement]))
        }
      }
    }
  }
}
