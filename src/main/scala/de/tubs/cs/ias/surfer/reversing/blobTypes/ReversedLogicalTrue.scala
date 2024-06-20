package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedLogicalTrue() extends ReversedBlob {

  override val children: List[ReversedBlob] = List()

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = "T"

}
