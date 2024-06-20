package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.util.edgeFollowsVar
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal
import wvlet.log.LogSupport

/** Reversing rule for ROPE_INIT
  *
  * @author Malte Wessels
  *
  */
object RopeInit extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ROPE_INIT"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    // we get the argument node of init
    val arg_node = current.getCpgCall.argument.filter(_.order == 1).l.head
    // and match based on its class
    arg_node match {
      // if it is a literal we can simply use the code/value of the literal
      case l: Literal =>
        List(ReversedLiteral(l.code))
      case i: Identifier =>
        // if it is an identifier
        val new_part_var_name = i.code
        // we have to find all incoming data dependency edges of this identifier
        val new_part_nodes = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(edgeFollowsVar(_, new_part_var_name))
          .l
          .map(_.out())
        // and reverse each edge individually
        new_part_nodes.flatMap { new_part_node =>
          ReversingRuleDeployer.deploy(new_part_node, context)
        }
      case x =>
        warn(s"unable to handle unexpected argument child of ROPE_INIT $x")
        List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN_CHILD_ROPE_INIT"))
    }
  }
}
