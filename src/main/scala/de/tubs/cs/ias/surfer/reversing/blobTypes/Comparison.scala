package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class Comparison(operator: String, lhs: ReversedBlob, rhs: ReversedBlob)
    extends ReversedBlob {
  override val children: List[ReversedBlob] = List(lhs, rhs)

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    s"${lhs.getBlobStringRepresentation} $operator ${rhs.getBlobStringRepresentation}"

}
