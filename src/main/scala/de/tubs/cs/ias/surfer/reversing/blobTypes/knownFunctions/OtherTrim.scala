package de.tubs.cs.ias.surfer.reversing.blobTypes.knownFunctions

import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedFCall
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

/** placeholder for rtrim, ltrim, chop
  *
  */
case class OtherTrim(override val name: String,
                     override val arguments: Map[String, ReversedBlob],
                     override val obj: Option[ReversedBlob])
    extends ReversedFCall {

  override def execute: ReversedBlob = children.head

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    execute.getBlobStringRepresentation

}
