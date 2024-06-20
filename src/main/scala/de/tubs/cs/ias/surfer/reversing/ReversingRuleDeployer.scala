package de.tubs.cs.ias.surfer.reversing

import de.tubs.cs.ias.cpg.slicing.representation.SliceNode
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedLoop
import de.tubs.cs.ias.surfer.reversing.rules.{FCall, FetchRWIS, Send}
import de.tubs.cs.ias.surfer.reversing.util.bytecodeFunctionCalls
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language._
import org.reflections.Reflections
import overflowdb.traversal.jIteratortoTraversal
import wvlet.log.LogSupport

import scala.collection.mutable
import scala.collection.mutable.{Map => MMap}
import scala.jdk.CollectionConverters.CollectionHasAsScala

/** object to index all ReversingRules and deploy them appropriately
  *
  * @author Simon Koch
  *
  */
object ReversingRuleDeployer extends LogSupport {

  /** constant representing the package of the Reversing trait
    */
  private val PACKAGE_NAME = "de.tubs.cs.ias.surfer.reversing"

  private val default_rule_usage = new mutable.HashMap[String, Int]()

  private val ruleStatistic: MMap[String, Int] = MMap()
  private var addCounter: Int = 0

  private def addRuleInvocation(rule: BytecodeReversingRule): Unit = {
    ruleStatistic.get(rule.BYTECODE_NAME) match {
      case Some(value) =>
        ruleStatistic.addOne(rule.BYTECODE_NAME -> (value + 1))
      case None =>
        ruleStatistic.addOne(rule.BYTECODE_NAME -> 1)
    }
    if (addCounter < 1000) {
      addCounter = addCounter + 1
    } else {
      addCounter = 0
      ruleStatistic.foreach {
        case (key, value) => println(s"$key -> $value")
      }
    }
  }

  def getDefaultRuleUsage: Map[String, Int] = {
    default_rule_usage.toMap
  }

  private val reflections: org.reflections.Reflections = new Reflections(
    PACKAGE_NAME)

  /** utility method to find all objects of a given class/trait
    *
    * @param cl the class
    * @tparam T the type of the object to be returned
    * @return
    */
  private def findAllObjects[T](cl: Class[T]): List[T] = {
    reflections
      .getSubTypesOf(cl)
      .asScala
      .map(_.getField("MODULE$").get(null).asInstanceOf[T])
      .toList
  }

  /** variable storing a map of bytecode name to BytecodeReversingRule
    */
  private val reversingRules: Map[String, BytecodeReversingRule] = {
    val interface =
      classOf[de.tubs.cs.ias.surfer.reversing.BytecodeReversingRule]
    findAllObjects(interface)
      .filter { rule =>
        rule.BYTECODE_NAME != DefaultReversingRule.BYTECODE_NAME
      }
      .map { rule =>
        rule.BYTECODE_NAME -> rule
      }
      .toMap
  }

  /** get all known rules
    *
    * @return the map of all known rules
    */
  def getRules: Map[String, BytecodeReversingRule] = reversingRules

  /** deploy the correct BytecodeReversingRule on the given SliceNode
    *
    * @param call the SliceNode to deploy on
    * @return the generated ReversedBlob
    */
  def deploy(call: SliceNode, context: ReversingContext): List[ReversedBlob] = {
    debug(s"deploying >${call.name}<")
    try {
      var currentContext = context.addVisitation(call.id())
      val rule: BytecodeReversingRule =
        if (reversingRules.contains(call.name)) {
          reversingRules(call.name)
        } else if (bytecodeFunctionCalls.contains(call.code)) {
          // remove all calls from the called method from the visited set
          // --> to allow multiple calls of the same functions
          val method =
            call.getCpgCall.out(EdgeTypes.CALL).collectAll[Method].l.head
          val to_remove = method.ast.collectAll[Call].map(_.id()).toSet.toSet
          currentContext = currentContext.removeNodes(to_remove)
          FCall
        } else if (call.code.startsWith("SEND_")) {
          Send
        } else if (call.code.startsWith("FETCH_")) {
          FetchRWIS
        } else {
          debug(s"using default rule for >${call.name}<")
          val prev = default_rule_usage.getOrElse(key = call.name, default = 0)
          default_rule_usage(call.name) = prev + 1
          DefaultReversingRule
        }
      //addRuleInvocation(rule)
      val ret = rule.reverse(call, currentContext)
      ret
    } catch {
      case _: AlreadyVisitedException =>
        debug("loop detected")
        List(ReversedLoop)
    }
  }

}
