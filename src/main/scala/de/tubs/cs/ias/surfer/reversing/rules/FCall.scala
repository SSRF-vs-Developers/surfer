package de.tubs.cs.ias.surfer.reversing.rules

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdge, SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedFCall
import de.tubs.cs.ias.surfer.reversing.util.cartesianProduct
import de.tubs.cs.ias.surfer.reversing.{BytecodeReversingRule, ReversedBlob, ReversingContext, ReversingRuleDeployer}
import wvlet.log.LogSupport

/** reversing rule for (any) function call
  *
  * @author Malte Wessels
  *
  */
object FCall extends BytecodeReversingRule with LogSupport {

  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    //println(s"reversing $current")
    val edges_return = current.inE(SliceEdgeTypes.RETURN_VAR)
    if (edges_return.nonEmpty) {
      //if we have any we go inter-function call
      // the one below works for one edge but adapts easy for more than one if that ever becomes the case
      edges_return.flatMap { ret =>
        ReversingRuleDeployer.deploy(ret.out(), context.pushStack(current))
      }
    } else {
      // get all incoming DATA_DEPENDENCY edges
      val edges = current.inE(SliceEdgeTypes.DATA_DEPENDENCE)
      // group the edges by the var they deliver and deploy them all
      val paramFollowUps = edges
        .filterNot(_.attributes.contains("obj"))
        .groupBy(_.getAttribute("arg").get)
        .map {
          case (name: String, edges: List[SliceEdge]) =>
            // deploy the different out nodes for the different var edges
            (name,
             edges
               .map(_.out())
               .flatMap(ReversingRuleDeployer.deploy(_, context)))
        }
      val objs = edges.find(_.attributes.contains("obj")) match {
        case Some(value) =>
          ReversingRuleDeployer.deploy(value.out(), context)
        case None => List()
      }
      //println(s"we got an object ${objs}")
      // create the cartesian product across all combinations of parameter origins
      val allParamFollowUpCombinations: Seq[Seq[ReversedBlob]] =
        cartesianProduct(paramFollowUps.values.toSeq)
      // create a ReversedFCall for every such combination
      allParamFollowUpCombinations.flatMap { combination: Seq[ReversedBlob] =>
        val zipedWithParameters: Map[String, ReversedBlob] =
          paramFollowUps.keys.zip(combination).toMap
        objs match {
          case Nil =>
            List(ReversedFCall(current.name, zipedWithParameters, None))
          case multiple =>
            multiple.map(obj =>
              ReversedFCall(current.name, zipedWithParameters, Some(obj)))
        }
      }.toList
    }
  }

  override val BYTECODE_NAME: String = "MORE_THAN_YOU_EXPECT"
}
