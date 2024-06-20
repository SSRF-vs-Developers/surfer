package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

case class ReversedObject(attributes: Map[String, ReversedBlob],
                          oType: Option[String] = None)
    extends ReversedBlob {

  def getType: String = oType match {
    case Some(value) => value
    case None        => "¯\\_(ツ)_/¯"
  }

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = {
    attributes
      .map {
        case (key, value) => s"$key => ${value.getBlobStringRepresentation}"
      }
      .mkString(s"[$getType:", ",", "]")
  }

  override val children: List[ReversedBlob] = attributes.values.toList
}
