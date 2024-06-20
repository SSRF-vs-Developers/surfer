package de.tubs.cs.ias.surfer

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call

import scala.jdk.CollectionConverters.IteratorHasAsScala

object CallFilter {

  def getGlobalReads(implicit cpg: Cpg): List[Call] = {
    cpg.graph.nodes.asScala
      .filter { node =>
        node.isInstanceOf[Call] &&
        (
          node.asInstanceOf[Call].code.matches("FETCH_R.*global.*") ||
          node.asInstanceOf[Call].code.matches("FETCH_FUNC_ARG.*global.*")
        )
      }
      .map(_.asInstanceOf[Call])
      .toList
  }

  def filterCalls(calls: Set[String])(implicit cpg: Cpg): List[Call] = {
    cpg.graph.nodes.asScala
      .filter(_.isInstanceOf[Call])
      .map(_.asInstanceOf[Call])
      .filter(call => calls.contains(call.name))
      .toList
  }

}
