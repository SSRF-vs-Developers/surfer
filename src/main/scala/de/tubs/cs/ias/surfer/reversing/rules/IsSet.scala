package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedArrayAccess, ReversedFCall}
import de.tubs.cs.ias.surfer.reversing.implicits.OneableIterableOnce
import de.tubs.cs.ias.surfer.reversing.util.{cartesianProduct, followChild}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext}
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

/** reversing rule ASSIGN
  *
  * @author Simon Koch
  */
object IsSet extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "ISSET_ISEMPTY_CV"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext
  ): List[ReversedBlob] = {
    val paramPossibilites = current.getCpgCall.astChildren.l
      .sortBy(_.order)
      .drop(1)
      .map(followChild(current, _, context))
    val states = cartesianProduct(paramPossibilites)
    states
      .map(
        state =>
          ReversedFCall(
            current.getCpgCall.argument
              .filter(_.order == 0)
              .collectAll[Literal]
              .one
              .code,
            state match {
              case one :: two :: Nil if current.code.contains("_DIM_") =>
                Map("1" -> ReversedArrayAccess(one, two))
              case _ =>
                state.zipWithIndex.map {
                  case (blob, index) =>
                    (index.toString, blob)
                }.toMap
            },
            None
        ))
      .toList
  }
}
