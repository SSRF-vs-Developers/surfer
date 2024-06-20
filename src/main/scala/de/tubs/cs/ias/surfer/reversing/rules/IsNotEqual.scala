package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}
import wvlet.log.LogSupport

object IsNotEqual extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "IS_NOT_EQUAL"

  override protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] =
    IsEqual
      .reverseImplementation(current, context)
      .map(elem => de.tubs.cs.ias.surfer.reversing.blobTypes.Not(elem))
}
