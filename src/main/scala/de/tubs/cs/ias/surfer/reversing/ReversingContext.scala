package de.tubs.cs.ias.surfer.reversing

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode

case class AlreadyVisitedException() extends RuntimeException

/**
  *
  * @param visited   set of visited node ids
  * @param callStack call stack as a list of bytecode fcalls as ids, growing to the left, i.e. newest element on the left
  */
case class ReversingContext(visited: Set[Long],
                            callStack: List[SliceNode],
                            cache: ReversingCache) {

  def addVisitation(id: Long): ReversingContext = {
    if (visited.contains(id))
      throw AlreadyVisitedException()
    else
      ReversingContext(visited + id, callStack, cache)
  }

  def removeNodes(nodes: Set[Long]): ReversingContext = {
    ReversingContext(visited -- nodes, callStack, cache)
  }

  def pushStack(funccall: SliceNode): ReversingContext = {
    ReversingContext(visited, funccall :: callStack, cache)
  }

  def popStack(): ReversingContext = {
    ReversingContext(visited, callStack.drop(1), cache)
  }

  def getStackHash: Int = callStack.map(_.getCpgCall.id()).hashCode()

}
