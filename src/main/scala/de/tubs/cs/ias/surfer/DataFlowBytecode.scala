package de.tubs.cs.ias.surfer

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}

import scala.util.matching.Regex

case class DataFlowBytecode(name: String, code: String, arg: Int, args: Int) {

  def getRepresentation: String = s"$name($arg/$args)"

}

object DataFlowBytecode {

  /** DFS to get all bytecode counts leading to specific sources/code on the path from the sink
    *
    * @param to the regexp describing the code segment we want to reach
    * @param candidate the SSRF candidate on which to run/whose sink we are going to use as a start
    * @return the set of DataFlowBytecode involved
    */
  def getInvolvedBytecodesTo(candidate: SSRFCandidate,
                             to: Regex*): Map[DataFlowBytecode, Int] = {
    def collect(current: SliceNode,
                collected: List[DataFlowBytecode],
                visited: Set[Long]): List[DataFlowBytecode] = {
      if (!to.exists(_.matches(current.code))) {
        val edges = current.inE(SliceEdgeTypes.DATA_DEPENDENCE)
        edges.flatMap { edge =>
          val next = edge.in()
          if (visited.contains(next.id())) {
            List() // in this case we have a loop and will not reach the to
          } else {
            collect(
              next,
              DataFlowBytecode(current.name,
                               current.code,
                               edge.getAttribute("arg").getOrElse("-1").toInt,
                               edges.length) :: collected,
              visited + current.id())
          }
        }
      } else {
        collected
      }
    }
    collect(candidate.getSink, Nil, Set())
      .groupBy(elem => elem.getRepresentation)
      .map {
        case (_, values) => values.head -> values.length
      }
  }

}
