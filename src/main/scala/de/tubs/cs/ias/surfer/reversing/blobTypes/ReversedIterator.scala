package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedIterator(array: ReversedBlob) extends ReversedBlob {

  override val children: List[ReversedBlob] = List(array)

  override private[reversing] def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = {
    "?" + array.getBlobStringRepresentation
  }
}
