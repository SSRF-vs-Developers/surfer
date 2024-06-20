package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedTypeCheck, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

object TypeCheck extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "TYPE_CHECK"

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val ttype =
      current.getCpgCall.astChildren.order(0).next().asInstanceOf[Literal].code
    val incoming = current.in(SliceEdgeTypes.DATA_DEPENDENCE)
    if (incoming.isEmpty) {
      error("no children but there should be some")
      List(
        ReversedTypeCheck(ttype,
                          ReversedUnknown(AnalysisSteps.SLICER,
                                          "MISSING_EXPECTED_INCOMING_DATA")))
    } else {
      incoming
        .flatMap(elem => ReversingRuleDeployer.deploy(elem, context))
        .map { reversedInputBlob =>
          ReversedTypeCheck(ttype, reversedInputBlob)
        }
    }
  }
}
