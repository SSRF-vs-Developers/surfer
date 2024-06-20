package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._
import wvlet.log.LogSupport

/** reversing rule for SEND_
  *
  * @author Malte Wessels
  *
  */
object Send extends BytecodeReversingRule with LogSupport {
  override val BYTECODE_NAME: String = "SEND_"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    // we get the argument node of the SEND_
    val argNode = current.getCpgCall.argument.filter(_.order == 0).l.head
    // match over the type of the argument
    argNode match {
      case x: Literal => // if it is a literal
        // we can simply return the corresponding string value
        List(ReversedLiteral(x.code))
      case x: Identifier => // if it is an identifier
        // we follow all edges back belonging to that identifier
        val ret = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.getAttribute("var").contains(x.name))
          .l
          .map(_.out())
          .flatMap { nextNode =>
            // and reverse their corresponding value
            ReversingRuleDeployer.deploy(nextNode, context)
          }
        ret
      case x =>
        warn(s"unknown child argument of $current : $x")
        // I am attributing the issue to SURFER as we are making assumptions that are SLICER and CPG independent
        List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN-SEND-CHILD"))
    }
  }
}
