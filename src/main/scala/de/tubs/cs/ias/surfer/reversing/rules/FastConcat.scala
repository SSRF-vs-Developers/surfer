package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes._
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import wvlet.log.LogSupport

/** reversing rule for FAST_CONCAT
  *
  * @author Malte Wessels
  */
object FastConcat extends BytecodeReversingRule with LogSupport {

  override val BYTECODE_NAME: String = "FAST_CONCAT"

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext
  ): List[ReversedBlob] = {
    //println(0)
    // get the arguments of fast concat
    current.getCpgCall.argument.l.sortBy(_.order) match {
      case (first: Literal) :: (second: Literal) :: Nil => // if we fast concat two literals
        //println(1)
        // we can simply concatenate and be done
        List(ReversedLiteral(first.code + second.code))
      case (first: Identifier) :: (second: Identifier) :: Nil => // of we have two identifiers
        //println(2)
        // we have to reverse either
        val firstCandidates =
          reverseEdgeOfName(current, first.name, context)
        val secondCandidates =
          reverseEdgeOfName(current, second.name, context)
        (firstCandidates.isEmpty, secondCandidates.isEmpty) match {
          case (true, true) =>
            warn("we have two empty reversings in FAST_CONCAT")
            List(ReversedLiteral(""))
          case (false, true) =>
            warn("we have lhs empty reversings in FAST_CONCAT")
            secondCandidates
              .map(x => ReversedConcat(List(x)))
          case (true, false) =>
            warn("we have rhs empty reversings in FAST_CONCAT")
            firstCandidates
              .map(x => ReversedConcat(List(x)))
          case (false, false) =>
            firstCandidates.flatMap { first =>
              secondCandidates.map { second =>
                ReversedConcat(
                  List(first, second)
                )
              }
            }
        }
      // and then concatenate them appropriately
      case (first: Identifier) :: (second: Literal) :: Nil => // if only the first is a identifier
        //println(3)
        // we only have to reverse the first
        val reversing = reverseEdgeOfName(current, first.name, context)
        if (reversing.isEmpty) {
          warn("reconstruction of lhs was empty")
          List(ReversedLiteral(second.code))
        } else {
          reversing.map { reversed =>
            ReversedConcat(
              List(
                reversed,
                ReversedLiteral(second.code)
              )
            )
          }
        }
      case (first: Literal) :: (second: Identifier) :: Nil => // if only the second is an identifier
        //println(4)
        // we only have to reverse the second
        val reversing = reverseEdgeOfName(current, second.name, context)
        if (reversing.isEmpty) {
          warn("reconstruction of rhs was empty")
          List(ReversedLiteral(first.code))
        } else {
          reversing.map { reversed =>
            ReversedConcat(
              List(
                ReversedLiteral(first.code),
                reversed
              )
            )
          }
        }
      case x =>
        //println(5)
        throw new RuntimeException(
          s"we have an unexpected mix of argument children of $current : $x"
        )
    }
  }

  private def reverseEdgeOfName(
      from: SliceNode,
      name: String,
      context: ReversingContext
  ): List[ReversedBlob] = {
    from
      .inE(SliceEdgeTypes.DATA_DEPENDENCE)
      .filter(_.getAttribute("var").contains(name))
      .flatMap { edge =>
        ReversingRuleDeployer.deploy(edge.out(), context)
      }
  }
}
