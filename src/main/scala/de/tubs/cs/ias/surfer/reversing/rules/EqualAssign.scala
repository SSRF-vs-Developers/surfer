package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}
import wvlet.log.LogSupport

/** reversing rule ASSIGN
  *
  * @author Simon Koch
  *
  */
object EqualAssign extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "="

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] =
    Assign.reverseImplementation(current, context)

}
