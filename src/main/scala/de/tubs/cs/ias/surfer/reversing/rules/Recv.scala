package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import wvlet.log.LogSupport

/** Reversing Rule for RECV
  *
  * @author Malte Wessels
  *
  */
object Recv extends BytecodeReversingRule with LogSupport {
  override val BYTECODE_NAME: String = "RECV"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    // get all incoming SEND_VAR dependency edges
    val edges_send_var = current.inE(SliceEdgeTypes.SEND_VAR)
    if (!(edges_send_var.sizeIs > 0)) {
      warn(
        s"RECV reversing expected > 0 SEND_VAR edges, got ${edges_send_var.size}")
      return List(ReversedUnknown(AnalysisSteps.SLICER, "NO_SEND_VAR"))
    }
    if (edges_send_var.size == 1) { // if the SEND_VAR is unique no further black reversing magic required
      // we can simply follow that edge and be done
      ReversingRuleDeployer.deploy(edges_send_var.head.out(), context)
    } else { // if we have multiple edges we have to ensure that we follow back the right one
      //val (previous_fcall, new_context): (SliceNode, ReversingContext) = ??? // context.popFcall()
      if (context.callStack.nonEmpty) {
        val previous_fcall = context.callStack.head
        val new_context = context.popStack()
        // this gives us all intra slice data dependence edges, i.e., the used send val nodes by the call
        val data_dependencies =
          previous_fcall.in(SliceEdgeTypes.DATA_DEPENDENCE).toSet
        // get all potential nodes based on send edges
        val send_edge_nodes = edges_send_var.map(_.out()).toSet
        // the intersection between those two sets are the nodes
        data_dependencies.intersect(send_edge_nodes).toList.flatMap {
          send_node => // we need to follow up on
            ReversingRuleDeployer.deploy(send_node, new_context)
        }
      } else {
        current
          .in(SliceEdgeTypes.SEND_VAR)
          .flatMap(sliceNode =>
            ReversingRuleDeployer.deploy(sliceNode, context))
      }
    }
  }
}
