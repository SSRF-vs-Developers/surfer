package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes._
import de.tubs.cs.ias.surfer.reversing.implicits.OneableIterableOnce
import de.tubs.cs.ias.surfer.reversing.util.edgeFollowsVar
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal
import wvlet.log.LogSupport

/** reversing rule for ROPE_ADD
  *
  * @author Malte Wessels
  *
  */
object RopeAdd extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ROPE_ADD"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext
  ): List[ReversedBlob] = {
    // get the value name to which we are roping
    val previous_rope_var_name =
      current.getCpgCall.argument.filter(_.order == 1).code.l.head
    // get the corresponding previous variable node
    val rope_node =
      current
        .inE(SliceEdgeTypes.DATA_DEPENDENCE)
        .filter(edgeFollowsVar(_, previous_rope_var_name))
        .one
        .out()
    // reverse the string value option(s)
    val rope_values: List[ReversedBlob] =
      ReversingRuleDeployer
        .deploy(rope_node, context)
    //.map(_.getBlobStringRepresentation)
    // get the argument that is added to the roped variable
    val arg_node = current.getCpgCall.argument.filter(_.order == 2).l.head
    // check its type
    arg_node match {
      case x: Literal => // if it is a literal
        // simple add its value to all the current string value options
        rope_values.map(rope_value =>
          ReversedConcat(List(rope_value, ReversedLiteral(x.code))))
      case x: Identifier => // if it is an identifier
        // get the name of the identifier
        val new_part_var_name = x.code
        // and get all possible previous nodes
        val new_part_nodes = current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(edgeFollowsVar(_, new_part_var_name))
          .l
          .map(_.out())
        // deploy over all those previous nodes
        val new_part_values = new_part_nodes
          .flatMap(new_part_node =>
            ReversingRuleDeployer.deploy(new_part_node, context))
        // concatenate all the rope and new part values
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
        warn(s"unexpected child argument for $current : $x")
        List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN_ROPE_ADD_CHILD"))
    }
    //ReversedString(rope_value + new_part_value)
  }
}
