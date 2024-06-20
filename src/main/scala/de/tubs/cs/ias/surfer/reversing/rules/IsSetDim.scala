package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}
import wvlet.log.LogSupport

/** reversing rule ASSIGN
  *
  * @author Simon Koch
  */
object IsSetDim extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ISSET_ISEMPTY_DIM_OBJ"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext
  ): List[ReversedBlob] = IsSet.reverseImplementation(current, context)
}
