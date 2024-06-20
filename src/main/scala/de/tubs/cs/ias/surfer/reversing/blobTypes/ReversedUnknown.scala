package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps.AnalysisStep
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

/** This implementation of ReversedBlob represents an element we are unable to reverse
  *
  * @author Simon Koch
  *
  *         ¯\_(ツ)_/¯
  *
  */
case class ReversedUnknown(errorStep: AnalysisStep, identifier: String)
    extends ReversedBlob {

  override val children: List[ReversedBlob] = List()

  private def getAnalysisStepString: String = errorStep match {
    case de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps.CPG =>
      "CPG"
    case de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps.SLICER =>
      "SLICE"
    case de.tubs.cs.ias.surfer.reversing.blobTypes.ReversedUnknown.AnalysisSteps.SURFER =>
      "SURFER"
  }

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String =
    s"(UKN_${getAnalysisStepString}_$identifier)"

}

object ReversedUnknown {

  object AnalysisSteps extends Enumeration {
    type AnalysisStep = Value
    val CPG, SLICER, SURFER = Value
  }

}
