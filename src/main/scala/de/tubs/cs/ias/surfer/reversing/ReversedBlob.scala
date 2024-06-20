package de.tubs.cs.ias.surfer.reversing

import de.tubs.cs.ias.surfer.reversing.blobTypes._
import wvlet.log.LogSupport

import java.util.UUID

trait ReversedBlob extends LogSupport {

  private val IDENT = UUID.randomUUID().toString

  private var controlDependency: Option[ReversedBlob] = None

  def execute: ReversedBlob = this

  def addControlDependency(blob: ReversedBlob): ReversedBlob = {
    this.controlDependency match {
      case None =>
        controlDependency = Some(blob)
      case Some(matchedControlDependency) =>
        controlDependency = Some(
          And(List(matchedControlDependency) ++ List(blob): _*))
    }
    this
  }

  val children: List[ReversedBlob]

  private[reversing] def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String

  def getBlobStringRepresentation(implicit cache: ReversingCache): String = {
    cache.withBlobStringRepresentationcache { blobCache =>
      blobCache.get(this) match {
        case Some(value) => value
        case None        => blobCache.add(this -> getBlobStringRepresentationInternal)
      }
    }
  }
}
