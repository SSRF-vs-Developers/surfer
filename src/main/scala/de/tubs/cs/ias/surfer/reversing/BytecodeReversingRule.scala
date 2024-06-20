package de.tubs.cs.ias.surfer.reversing

import de.tubs.cs.ias.cpg.slicing.representation.{SliceEdge, SliceEdgeTypes, SliceNode}
import de.tubs.cs.ias.surfer.reversing.blobTypes.{And, Not, Or, ReversedLogicalTrue}
import wvlet.log.LogSupport

trait BytecodeReversingRule extends LogSupport {

  val BYTECODE_NAME: String

  /** this is the individual implementation of each bytecode that shall be reversed
    *
    * @param current the current slice node that is supposed to be reversed
    * @param context the context in which the reversing is happening
    * @return the different blobs we can reverse from the current slice node
    */
  protected def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob]

  /** this method does the actual full reversing covering data and control dependence
    *
    * @param current the current slice node we want to reverse
    * @param context the context in which the reversing is happening
    * @return the set of blobs that can be reversed from the current slice node
    */
  final def reverse(current: SliceNode,
                    context: ReversingContext): List[ReversedBlob] = {
    context.cache.withReversingCache { cache =>
      cache.get((this, current.getCpgCall.id(), context.getStackHash)) match {
        case Some(value) => value
        case _ =>
          val reversedContent = reverseImplementation(current, context)
          val reversedCondition = deployOnControlDependency(current, context)
          val ret = reversedContent.map { reversed =>
            reversed.addControlDependency(reversedCondition)
          }
          cache.add(
            (this, current.getCpgCall.id(), context.getStackHash) -> ret)
      }
    }
  }

  /** this function covers reversing the control dependence edge (if there is any)
    *
    * @param current the slice node for which we want to calculate possible control dependence
    * @param context the context of the reconstruction of the control dependence
    * @return the reversed blob representing the required condition for the current slice node
    */
  protected def deployOnControlDependency(
      current: SliceNode,
      context: ReversingContext): ReversedBlob = {
    current.inE(SliceEdgeTypes.CONTROL_DEPENDENCE) match {
      case Nil => ReversedLogicalTrue()
      case single :: Nil =>
        Or(
          ReversingRuleDeployer
            .deploy(single.out(), context)
            .map(negateIfRequired(_, single)))
      case multiple =>
        And(
          multiple.flatMap(
            cond =>
              ReversingRuleDeployer
                .deploy(cond.out(), context)
                .map(negateIfRequired(_, cond))): _*
        )
    }
  }

  /** function to negate reversed control flow condition of required
    *
    * @param blob the reversed control flow condition
    * @param single the edge representing the control flow
    * @return the (reversed) blob
    */
  private def negateIfRequired(blob: ReversedBlob,
                               single: SliceEdge): ReversedBlob = {
    single.attributes.get("fallthrough") match {
      case Some("true")  => blob
      case Some("false") => Not(blob)
      case Some(x) =>
        error(s"'$x'-fallthrough arg for CDG-Edge. Slicer-Bug. Ignoring it..")
        blob
      case None => blob
    }
  }
}

/** Default Reversing Rule in case no other rule matches
  *
  * @author Simon Koch
  *
  */
object DefaultReversingRule extends BytecodeReversingRule {

  override val BYTECODE_NAME: String = "DEFAULT"

  /** in case no other reversing rule matches we treat each edge as an individual reverse
    *
    * @param current the current node to apply the reversing to
    * @param context the context in which the reversing is happening
    * @return the list of reversed blobs
    */
  override def reverseImplementation(
      current: SliceNode,
      context: ReversingContext): List[ReversedBlob] = {
    val edges = current
      .in(SliceEdgeTypes.DATA_DEPENDENCE)
    edges.flatMap { edge =>
      ReversingRuleDeployer.deploy(edge, context)
    }
  }
}
