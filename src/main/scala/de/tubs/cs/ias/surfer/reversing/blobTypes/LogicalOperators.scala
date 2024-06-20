package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

import scala.collection.mutable.ListBuffer

case class Not(value: ReversedBlob) extends ReversedBlob {

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    s"!$getBlobStringRepresentation"

  override val children: List[ReversedBlob] = List(value)
}

/** Class representing a logical and connection
  *
  * DO NOT USE THIS CONSTRUCTOR - use the companion object apply
  *
  * @param children The conditions connected via and
  * @param proper this has to be true and is set by the companion object apply
  */
case class And private (override val children: List[ReversedBlob],
                        proper: Boolean)
    extends ReversedBlob {

  assert(proper, "you need to use the apply function  with  Or( list : _*)")

  override def toString: String = children.mkString("And(", ",", ")")

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    children.map(_.getBlobStringRepresentation).toSet.mkString(" and ")

}

object And {

  def apply(conds: ReversedBlob*): ReversedBlob = {
    val collected: ListBuffer[ReversedBlob] = ListBuffer()
    conds.foreach {
      case And(values, _) => collected.addAll(values)
      case otherwise      => collected.addOne(otherwise)
    }
    val ret =
      collected.filterNot(_.isInstanceOf[ReversedLogicalTrue]).toList match {
        case Nil           => ReversedLogicalTrue()
        case single :: Nil => single
        case multiple =>
          And(multiple, proper = true)
      }
    ret
  }

}

/** Class representing a logical or connection
  *
  * DO NOT USE THIS CONSTRUCTOR - use the companion object apply
  *
  * @param children The conditions connected via and
  * @param proper this has to be true and is set by the companion object apply
  */
case class Or(override val children: List[ReversedBlob], proper: Boolean)
    extends ReversedBlob {

  assert(proper, "you need to use the apply function  with  Or( list : _*)")

  override def toString: String = children.mkString("Or(", ",", ")")

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    children.map(_.getBlobStringRepresentation).toSet.mkString(" or ")

}

object Or {

  def apply(conds: List[ReversedBlob]): ReversedBlob = {
    val collected: ListBuffer[ReversedBlob] = ListBuffer()
    conds.foreach {
      case Or(values, _) => collected.addAll(values)
      case otherwise     => collected.addOne(otherwise)
    }
    val ret =
      if (collected.exists(elem => elem.isInstanceOf[ReversedLogicalTrue])) {
        ReversedLogicalTrue()
      } else {
        collected.toList match {
          case Nil           => ReversedUnknown(AnalysisSteps.SURFER, "OR_NO_CHILDREN")
          case single :: Nil => single
          case multiple      => Or(multiple, proper = true)
        }
      }
    ret
  }

}

/**
  * This represents IS_EQUAL and IS_IDENTICAL and their negated counterparts.
  * I.e. ==, !=, ===, and !==
  */
case class Equals(override val children: List[ReversedBlob])
    extends ReversedBlob {

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = {
    children
      .map(_.getBlobStringRepresentation)
      .toSet
      .mkString(" == ")
  }

}

object Equals {

  def apply(values: ReversedBlob*): Equals = Equals(values.toList)

}
