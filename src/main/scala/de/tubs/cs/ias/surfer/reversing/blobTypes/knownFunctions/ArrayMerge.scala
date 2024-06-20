package de.tubs.cs.ias.surfer.reversing.blobTypes.knownFunctions

import de.tubs.cs.ias.surfer.reversing.blobTypes.{LiteralTypes, ReversedArrayElement, ReversedFCall, ReversedLiteral}
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

case class ArrayMerge(override val name: String,
                      override val arguments: Map[String, ReversedBlob],
                      override val obj: Option[ReversedBlob])
    extends ReversedFCall {

  private val UNIQUE_NUMBER = UUID.randomUUID().toString

  override def execute: ReversedBlob = {
    val reversedArray: ListBuffer[(ReversedBlob, ReversedBlob)] = ListBuffer()

    @tailrec
    def addAllArrayElements(ae: ReversedArrayElement): Unit = {
      ae.index match {
        case rl: ReversedLiteral
            if (rl.getLiteralType == LiteralTypes.NUMBER && rl.value.toInt >= 0) || rl.value == "NEXT" =>
          reversedArray.addOne(ReversedLiteral(UNIQUE_NUMBER) -> ae.value)
        case _ =>
          reversedArray.addOne(ae.index -> ae.value)
      }

      ae.previous match {
        case Some(value: ReversedArrayElement) => addAllArrayElements(value)
        case Some(value: ReversedBlob) =>
          reversedArray.addOne(ReversedLiteral(UNIQUE_NUMBER) -> value)
        case None =>
      }
    }

    arguments.toList.sortBy(_._1).reverse.map(_._2.execute).foreach {
      case ae: ReversedArrayElement =>
        addAllArrayElements(ae)
      case blob: ReversedBlob =>
        reversedArray.addOne(ReversedLiteral(UNIQUE_NUMBER) -> blob)
    }

    def createNewArray(pairs: List[(ReversedBlob, ReversedBlob)],
                       counter: Int = 0): Option[ReversedArrayElement] = {
      pairs match {
        case Nil => None
        case (numIndex: ReversedLiteral, blob) :: rest
            if numIndex.value == UNIQUE_NUMBER =>
          Some(
            ReversedArrayElement(ReversedLiteral(counter.toString),
                                 blob,
                                 createNewArray(rest, counter + 1)))
        case (index, blob) :: rest =>
          Some(ReversedArrayElement(index, blob, createNewArray(rest, counter)))
      }
    }

    val array = createNewArray(reversedArray.toList.reverse).get
    array
  }

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    execute.getBlobStringRepresentation

}
