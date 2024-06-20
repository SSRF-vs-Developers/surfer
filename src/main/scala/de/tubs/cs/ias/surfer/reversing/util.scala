package de.tubs.cs.ias.surfer.reversing

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdge, SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedLiteral
import de.tubs.cs.ias.surfer.reversing.implicits.OneableIterableOnce
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

object implicits {
  implicit class OneableIterableOnce[T](l: IterableOnce[T]) {
    def one: T = {
      l.knownSize match {
        case -1 =>
          val iterator = l.iterator
          assert(iterator.hasNext, "expected one in IterableOnce, got 0")
          val tmp = iterator.next()
          assert(!iterator.hasNext, "expected one in IterableOnce, got more")
          tmp
        case x =>
          assert(x == 1,
                 "expected one in IterableOnce, got " + x + " (knownSize)")
          l.iterator.next()
      }
    }
  }
  implicit class SlideDDGEdge(e: SliceEdge) {
    def argName: String = {
      assert(e.label == SliceEdgeTypes.DATA_DEPENDENCE,
             s"trying to get var of non-DDG-edge $e")
      e.getAttribute("arg").get
    }
  }
}

object util {
  def followChild(current: SliceNode,
                  child: AstNode,
                  context: ReversingContext): List[ReversedBlob] = {
    // info(s"following child $child")
    child match {
      case x: Literal =>
        List(ReversedLiteral(x.code))
      case x: Identifier =>
        current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(edgeFollowsVar(_, x.code))
          .map(_.out())
          .flatMap(ReversingRuleDeployer.deploy(_, context))
      case weird =>
        throw new RuntimeException(
          s"we expect either identifier or literal for $current not $weird")
    }
  }

  def edgeFollowsVar(edge: SliceEdge, arg: String): Boolean = {
    assert(edge.label == SliceEdgeTypes.DATA_DEPENDENCE,
           "edgeFollowsVar called with non-data edge")
    val x = edge.getAttribute("var").get
    (x.startsWith("V") || x.startsWith("T")) && arg == x || // match T,V vars
    arg == s"CV($$$x)" // match compiled (CV) php vars
  }

  val bytecodeFunctionCalls: Array[String] =
    Array("DO_FCALL", "DO_ICALL", "DO_UCALL", "DO_FCALL_BY_NAME")

  /**
    * @author Malte Wessels
    */
  def parameterNameToIndex(name: String, method: Method): Int = {
    method.parameter.nameExact(name).order.one + 1
  }

  /**
    * Get the parameter index for a named parameter from it's send.
    * Throws if the called function is an internal function.
    *
    * @author Malte Wessels
    * @param send the send bytecode
    * @return the index
    */
  def getParameterIndexFromSend(send: Call): Int =
    send.out(EdgeTypes.ARGUMENT).collectAll[Literal].one.code match {
      case number if number.forall(Character.isDigit) => number.toInt
      case string =>
        val method = send
          .in(EdgeTypes.ARGUMENT)
          .collectAll[Call]
          .one
          .out(EdgeTypes.CALL)
          .collectAll[Method]
          .one
        method.code match {
          case "INTERNAL_FUNCTION" =>
            throw new RuntimeException(
              "INTERNAL_FUNCTION can't be used for parameter name to position mapping")
          case _ => parameterNameToIndex(string, method)
        }
    }

  // copied from https://stackoverflow.com/a/50105448/17330209
  def cartesianProduct[T](seqs: Seq[Seq[T]]): Seq[Seq[T]] = {
    seqs.foldLeft(Seq(Seq.empty[T]))((b, a) =>
      b.flatMap(i => a.map(j => i ++ Seq(j))))
  }
}
