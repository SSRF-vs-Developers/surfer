package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

/** This implementation of the ReversedBlob represents an Array
  *
  * @author Simon Koch
  * @param index : the blob representing the index of the element
  * @param value : the value representing the value of the array element
  * @param previous : the previous element in the reversing chain of this array
  */
case class ReversedArrayElement(index: ReversedBlob,
                                value: ReversedBlob,
                                previous: Option[ReversedBlob])
    extends ReversedBlob {

  override val children: List[ReversedBlob] =
    List(Some(index), Some(value), previous).filter(_.nonEmpty).map(_.get)

  def createPairedList: List[(ReversedBlob, ReversedBlob)] = {
    (index, value) :: (previous match {
      case None => Nil
      case Some(array: ReversedArrayElement) =>
        array.createPairedList
      case Some(blob) => (ReversedLiteral("NEXT"), blob) :: Nil
    })
  }

  def createPairedBlobList(
      implicit cache: ReversingCache): List[(String, String)] =
    createPairedList.map {
      case (a, b) =>
        (a.getBlobStringRepresentation, b.getBlobStringRepresentation)
    }

  /*override def getArrayBlobStringInternalRepresentations(
      implicit cache: ReversingCache): List[(String, String)] = {
    previous match {
      case Some(value: ReversedBlob) =>
        println(s"${index.getBlobStringRepresentation} -> ${value.getBlobStringRepresentation}")
        (index.getBlobStringRepresentation, value.getBlobStringRepresentation) :: value.getArrayBlobStringInternalRepresentations
      case None =>
        (index.getBlobStringRepresentation, value.getBlobStringRepresentation) :: Nil
    }
  }*/

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    createPairedBlobList.mkString("[", ",", "]")
  //getArrayBlobStringInternalRepresentations.mkString("[", ",", "]")

}

object ReversedArrayElement {
  def createFromMap(
      map: Map[ReversedBlob, ReversedBlob]): Option[ReversedArrayElement] = {
    map match {
      case map if map.sizeIs == 0 => None
      case map if map.sizeIs == 1 =>
        Some(ReversedArrayElement(map.head._1, map.head._2, None))
      case map if map.sizeIs > 1 =>
        var prev = ReversedArrayElement(map.head._1, map.head._2, None)
        map
          .drop(1)
          .foreach(n => prev = ReversedArrayElement(n._1, n._2, Some(prev)))
        Some(prev)
    }
  }
}
