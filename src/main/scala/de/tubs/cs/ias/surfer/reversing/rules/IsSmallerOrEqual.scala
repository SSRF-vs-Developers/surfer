package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.blobTypes.Comparison
import de.tubs.cs.ias.surfer.reversing.util.followChild
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

object IsSmallerOrEqual extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "IS_SMALLER_OR_EQUAL"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val lhs :: rhs :: Nil = current.getCpgCall.astChildren.l.sortBy(_.order)
    val reversedLhs = followChild(current, lhs, context)
    val reversedRhs = followChild(current, rhs, context)
    reversedLhs.flatMap { lhs =>
      reversedRhs.map { rhs =>
        Comparison("<=", lhs, rhs)
      }
    }
  }
}
