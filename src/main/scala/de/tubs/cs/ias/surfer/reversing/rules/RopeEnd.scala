package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes._
import de.tubs.cs.ias.surfer.reversing.util.edgeFollowsVar
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal
import wvlet.log.LogSupport

/** Reversing rule for ROPE_END
  *
  * @author Malte Wessels
  */
object RopeEnd extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ROPE_END"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    // get the name of the roped variable
    val previous_rope_var_name =
      current.getCpgCall.argument.filter(_.order == 1).code.l.head
    // get the name of the added literal or identifier
    val new_part_arg = current.getCpgCall.argument.filter(_.order == 2).l.head
    // get the incoming data dependency edges
    val edges = current.inE(SliceEdgeTypes.DATA_DEPENDENCE)
    // filter the edges/nodes for which the related var matches the roped string variable
    val rope_nodes =
      edges.filter(edgeFollowsVar(_, previous_rope_var_name)).map(_.out())
    // get the possible resulting strings
    val rope_values = rope_nodes
      .flatMap(ReversingRuleDeployer.deploy(_, context))
    // check the type of the added variable
    new_part_arg match {
      case x: Literal => // if it is a literal simply add it
        rope_values.map(
          value =>
            ReversedConcat(
              List(value, ReversedLiteral(x.code))
          ))
      case x: Identifier => // if it is an identifier we have to reconstruct its possible value(s)
        // get the name of the identifier
        val new_part_var_name = x.code
        // get all incoming data dependency edges belonging to that edge and their out nodes
        val new_part_nodes = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(edgeFollowsVar(_, new_part_var_name))
          .l
          .map(_.out())
        // get all the resulting value options
        val new_part_values = new_part_nodes.flatMap { new_part_node =>
          ReversingRuleDeployer
            .deploy(new_part_node, context)
        }
        // then combine them with each other
        rope_values.flatMap { rope_value =>
          new_part_values.map { new_part_value =>
            (rope_value, new_part_value) match {
              case (o: ReversedLiteral, n: ReversedLiteral) =>
                ReversedLiteral(o.value + n.value)
              case _ =>
                ReversedConcat(
                  List(rope_value, new_part_value)
                )
            }
          }
        }
      case x =>
        warn(s"unexpected argument child of $current : $x")
        List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN_ROPE_END_CHILD"))
    }
  }
}
