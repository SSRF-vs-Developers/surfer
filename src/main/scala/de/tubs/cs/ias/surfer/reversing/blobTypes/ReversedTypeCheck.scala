package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedTypeCheck(ttype: String, variable: ReversedBlob)
    extends ReversedBlob {

  override val children: List[ReversedBlob] = List(variable)

  private def getCleanType: String = {
    ttype.substring(1, ttype.length - 1)
  }

  override private[reversing] def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    throw new RuntimeException(
      "a type check does not make sense as blob string representation")
}
