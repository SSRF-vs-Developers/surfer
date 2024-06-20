package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedConcat(override val children: List[ReversedBlob])
    extends ReversedBlob {

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    children.map(_.getBlobStringRepresentation).mkString("")

}
