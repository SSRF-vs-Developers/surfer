package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}

object IsIdentical extends BytecodeReversingRule {
  override val BYTECODE_NAME: String = "IS_IDENTICAL"

  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] =
    IsEqual.reverseImplementation(current, context)
}
