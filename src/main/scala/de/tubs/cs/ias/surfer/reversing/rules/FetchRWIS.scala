package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.Surfer
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.{ReversedArrayAccess, ReversedGlobal, ReversedLiteral, ReversedUnknown}
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.iterableToTraversal

/** reversing rule for FETCH_[R,W,RW,IS]
  *
  * @author Simon Koch
  *
  */
object FetchRWIS extends BytecodeReversingRule {

  override val BYTECODE_NAME: String = "FETCH_STUFF"

  private def fetchDimRWRWISG(current: SliceNode): List[ReversedBlob] = {
    List(
      ReversedGlobal( // if so return that as a reversed blob
        current.code
          .split(" \\(global\\) ")
          .last
          .replace("string(\"", "")
          .replace("\")", "")))
  }

  private def fetchDimFuncArg(current: SliceNode,
                              context: ReversingContext): List[ReversedBlob] = {
    val array :: index :: Nil =
      current.getCpgCall.astChildren.sortBy(_.order).toList
    val arrayBlobs: List[ReversedBlob] = current
      .inE(SliceEdgeTypes.DATA_DEPENDENCE)
      .filter(
        _.getAttribute("var").contains(array.asInstanceOf[Identifier].name))
      .flatMap(edge => ReversingRuleDeployer.deploy(edge.out(), context))
    val indexBlob: List[ReversedBlob] = index match {
      case x: Identifier =>
        current
          .inE(SliceEdgeTypes.DATA_DEPENDENCE)
          .filter(_.getAttribute("var").contains(x.name))
          .map(_.out())
          .flatMap(node => ReversingRuleDeployer.deploy(node, context))
      case x: Literal =>
        List(ReversedLiteral(x.code))
      case _ => throw new RuntimeException("")
    }
    (if (arrayBlobs.nonEmpty) arrayBlobs
     else
       List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN-ARRAY"))) //this could also be an issue of CPG or SLICER but we are assuming their correctness for now
      .flatMap { array =>
        (if (indexBlob.nonEmpty) indexBlob
         else
           List(ReversedUnknown(AnalysisSteps.SURFER, "UNKNOWN-INDEX"))) //this could also be an issue of CPG or SLICER but we are assuming their correctness for now
          .map { index =>
            ReversedArrayAccess(array, index)
          }
      }
  }

  private def fetchFuncArg(current: SliceNode,
                           context: ReversingContext): List[ReversedBlob] = {
    if ("FETCH_FUNC_ARG.*global.*".r.matches(current.code)) {
      val _ :: global :: Nil =
        current.getCpgCall.astChildren.sortBy(_.order).toList
      List(ReversedGlobal(global.asInstanceOf[Literal].code))
    } else {
      List(ReversedUnknown(AnalysisSteps.SURFER, "NON-GLOBAL-ARRAY"))
    }
  }

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] =
    if ("FETCH_(R|W|RW|IS).*global.*".r.matches(current.code)) {
      fetchDimRWRWISG(current)
    } else if ("FETCH_DIM_FUNC_ARG.*".r.matches(current.code)) {
      fetchDimFuncArg(current, context)
    } else if ("FETCH_FUNC_ARG.*".r.matches(current.code)) {
      fetchFuncArg(current, context)
    } else if ("FETCH_CLASS_CONST.*".r.matches(current.code)) {
      // cpg limitation: https://github.com/simkoc/php-cpg/issues/318
      List(ReversedUnknown(AnalysisSteps.CPG, "FETCH_CLASS_CONST"))
    } else if ("FETCH_CONSTANT.*".r.matches(current.code)) {
      // CPG limitation
      // Poor man's dataflow:
      // get all defines for this constant
      // if there is only one: use that.
      val const_name = current.getCpgCall.argument.order(0).l match {
        case name :: Nil => name.code
        case _ =>
          return List(
            ReversedUnknown(AnalysisSteps.SURFER,
                            "FETCH_CONSTANT_NAME_UNCLEAR"))
      }

      val fetch_consts = Surfer.cpg.get.call("define").l
      val fetch_consts_arg = fetch_consts
        .where(
          _.argument
            .order(1)
            .collectAll[Call]
            .argument
            .order(0)
            .code(const_name))
        .l
      val const_candidates = fetch_consts_arg.argument
        .order(2)
        .collectAll[Call]
        .argument
        .order(0)
        .code
        .l
      const_candidates match {
        case value :: Nil =>
          println()
          List(ReversedLiteral(value))
        case _ => List(ReversedUnknown(AnalysisSteps.CPG, "FETCH_CONSTANT"))
      }

    } else {
      // otherwise we have no clue what we do (for now say unknown)
      List(
        ReversedUnknown(AnalysisSteps.SURFER, s"UNKNOWN-FETCH-${current.name}"))
    }
}
