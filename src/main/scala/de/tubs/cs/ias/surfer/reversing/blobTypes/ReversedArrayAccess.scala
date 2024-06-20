package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps
import de.tubs.cs.ias.surfer.reversing.blobTypes.knownFunctions.ArrayMerge
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedArrayAccess(array: ReversedBlob, index: ReversedBlob)
    extends ReversedBlob {

  override val children: List[ReversedBlob] = List(array, index)

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = {
    val actualArray = array match {
      case fc: ArrayMerge => fc.execute
      case otherwise      => otherwise
    }
    (actualArray, index) match {
      case (array: Assignment, index) =>
        ReversedArrayAccess(array.from, index).getBlobStringRepresentation
      case (array: ReversedArrayElement, index: ReversedLiteral) =>
        if (array.index.getBlobStringRepresentation == index.value) {
          array.value.getBlobStringRepresentation
        } else {
          array.previous match {
            case Some(value) =>
              ReversedArrayAccess(value, index).getBlobStringRepresentation
            case None =>
              ReversedUnknown(AnalysisSteps.SURFER, "ARRAY_INDEX_UNKNOWN").getBlobStringRepresentation
          }
        }
      case (array: ReversedArrayElement, index) =>
        s"${array.getBlobStringRepresentation}[${index.getBlobStringRepresentation}]"
      case (otherwise, index) =>
        s"${otherwise.getBlobStringRepresentation}[${index.getBlobStringRepresentation}]"
    }
  }
}
