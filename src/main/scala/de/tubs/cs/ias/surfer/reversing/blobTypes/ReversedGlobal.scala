package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedGlobal(name: String) extends ReversedBlob {

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = s"<G:$name>"

  override val children: List[ReversedBlob] = List()
}
