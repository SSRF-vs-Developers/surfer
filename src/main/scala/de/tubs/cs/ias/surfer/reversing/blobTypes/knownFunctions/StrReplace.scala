package de.tubs.cs.ias.surfer.reversing.blobTypes.knownFunctions

import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedFCall
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class StrReplace(override val name: String,
                      override val arguments: Map[String, ReversedBlob],
                      override val obj: Option[ReversedBlob])
    extends ReversedFCall {

  override def execute: ReversedBlob = this

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = {
    if (children.lengthIs >= 2) {
      children(2).getBlobStringRepresentation
    } else {
      super.getBlobStringRepresentationInternal
    }
  }

}
