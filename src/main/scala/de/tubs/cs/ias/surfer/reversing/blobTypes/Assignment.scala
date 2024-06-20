package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class Assignment(from: ReversedBlob) extends ReversedBlob {

  override val children: List[ReversedBlob] = List(from)

  override def execute: ReversedBlob = from.execute

  override private[reversing] def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    from.getBlobStringRepresentation
}
