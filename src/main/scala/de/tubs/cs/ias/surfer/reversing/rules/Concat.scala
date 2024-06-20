package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}
import wvlet.log.LogSupport

/** reversing rule CONCAT
  *
  * @author Malte Wessels
  *
  */
object Concat extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "CONCAT"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] =
    FastConcat.reverseImplementation(current, context)
}
