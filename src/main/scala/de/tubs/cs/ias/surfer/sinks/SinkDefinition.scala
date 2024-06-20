package de.tubs.cs.ias.surfer.sinks

import de.tubs.cs.ias.cpg.slicing.algorithms.util.implicits.JumpToMethod
import de.tubs.cs.ias.surfer.reversing.util.bytecodeFunctionCalls
import io.joern.bytecode.util.extensions.NodeExtension.ExtendedCall
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, CfgNode, Identifier}
import io.shiftleft.semanticcpg.language._
import overflowdb.Edge
import overflowdb.traversal.jIteratortoTraversal

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.matching.Regex

class SinkDefinition(sinkName: String, sinkChild: Int) {

  def matches(call: Call): Boolean = {
    if (!bytecodeFunctionCalls.contains(call.code)) return false
    //val out = call.out(EdgeTypes.CALL).l
    if (call.methodFullNameFixed != sinkName) {
      return false
    }
    call.getParameter(sinkChild) match {
      case Some(_: Identifier) => return true
      case _                   =>
    }
    if (call.argument
          .collectAll[Call]
          .filter(_.name == "SEND_ARRAY")
          .exists(_.inE(EdgeTypes.REACHING_DEF).nonEmpty)) {
      // SEND_ARRAY with incoming data flow
      return true
    }
    false
    /*call.name == sinkName && call
      .getParameter(sinkChild)
      .isInstanceOf[Some[Identifier]]*/
  }

  private def getBackEdge(sendVar: Call, doCall: Call): Edge = {
    val inEdges = sendVar.inE(EdgeTypes.ARGUMENT).asScala.toList
    assert(
      inEdges.length == 1,
      s"SEND_VAR should only have a single in edge but has ${inEdges.length}")
    val fromNode = inEdges.head.outNode()
    assert(
      fromNode.id() == doCall.id(),
      s"the from node of SEND_VAR should be the initial DO_ but was ${fromNode.asInstanceOf[CfgNode].code}")
    inEdges.head
  }

  def getEdgeForSink(sink: Call): Option[Edge] = {
    assert(matches(sink),
           "passed non-sink to getEdgeForSink() which expects a sink")

    val param = sink.getParameter(sinkChild)
    param.astParent.nextOption() match {
      case Some(value) => Some(getBackEdge(value.asInstanceOf[Call], sink))
      case None        => None
    }
  }

  def getSinkName: String = sinkName

}

class CurlSetOptSinkDefinition() extends SinkDefinition("curl_setopt", 3) {
  val CURLOPT_URL = "CURLOPT_URL"

  var overapprox_counter = 0

  override def matches(call: Call): Boolean = {
    if (!super.matches(call)) return false
    // check if the option is the right one
    // debug this. one unit tests fails.
    call.argument
      .collectAll[Call]
      .l(1)
      .in(EdgeTypes.REACHING_DEF)
      .collectAll[Call]
      .argument
      .collectAll[Call]
      .filter(
        _.code.startsWith("FETCH_CONSTANT")
      )
      .argument
      .code
      .l
      .lastOption match {
      case Some(value) =>
        value == CURLOPT_URL || value.contains("\\") && value.endsWith(
          CURLOPT_URL)
      case None =>
        overapprox_counter += 1
        true // the traversal to the constant went wrong, keep it
    }
  }
}

class NativeSinkDefinition(val bytecode: Regex)
    extends SinkDefinition(bytecode.toString(), -1) {

  override def matches(call: Call): Boolean = {
    bytecode.matches(call.code)
  }

  override def getEdgeForSink(sink: Call): Option[Edge] = {
    assert(matches(sink),
           "passed non-sink to getEdgeForSink() which expects a sink")
    None
  }

}

object SinkDefinition {

  def getDefinitions: Set[SinkDefinition] = Set(
    new SinkDefinition("file_get_contents", 1),
    new SinkDefinition("curl_init", 1),
    new CurlSetOptSinkDefinition(),
    new SinkDefinition("getimagesize", 1),
    new SinkDefinition("get_headers", 1),
    // Wordpress Requests
    new SinkDefinition("wp_http::get", 1),
    new SinkDefinition("requests::get", 1),
    new SinkDefinition("wp_remote_request", 1),
    new SinkDefinition("wp_remote_get", 1),
    new SinkDefinition("wp_remote_post", 1),
    new SinkDefinition("wp_remote_head", 1),
    new SinkDefinition("wp_safe_remote_request", 1),
    new SinkDefinition("wp_safe_remote_head", 1),
    new SinkDefinition("wp_safe_remote_get", 1),
    new SinkDefinition("wp_safe_remote_post", 1)
  )

}
