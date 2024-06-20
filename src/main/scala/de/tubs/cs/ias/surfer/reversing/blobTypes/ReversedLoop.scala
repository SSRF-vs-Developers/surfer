package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

/** ReversedBlob indicating that we have gone full circle during reconstruction
  *
  * @author Simon Koch
  *
  */
object ReversedLoop extends ReversedBlob {

  override val children: List[ReversedBlob] = List()

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = "♾"

}
