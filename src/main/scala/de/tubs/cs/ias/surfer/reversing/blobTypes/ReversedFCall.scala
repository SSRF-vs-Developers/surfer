package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.blobTypes.knownFunctions._
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

trait ReversedFCall extends ReversedBlob {

  val name: String
  val arguments: Map[String, ReversedBlob]
  val obj: Option[ReversedBlob]

  override val children: List[ReversedBlob] = arguments.values.toList

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = {
    children.map(_.getBlobStringRepresentation).mkString("")
  }

  def execute: ReversedBlob

}

case class GenericReversedFCall(
    override val name: String,
    override val arguments: Map[String, ReversedBlob],
    override val obj: Option[ReversedBlob],
    executed: Boolean = false)
    extends ReversedFCall {

  override def execute: ReversedBlob = {
    assert(!executed)
    GenericReversedFCall(name, arguments, obj, executed = true)
  }

}

object ReversedFCall {

  def apply(name: String,
            arguments: Map[String, ReversedBlob],
            obj: Option[ReversedBlob]): ReversedFCall = {
    name match {
      case "str_replace" | "str_ireplace" => StrReplace(name, arguments, obj)
      case "preg_replace" | "preg_filter" => PregReplace(name, arguments, obj)
      case "trim"                         => Trim(name, arguments, obj)
      case "rtrim" | "ltrim" | "chop"     => OtherTrim(name, arguments, obj)
      case "array_merge"                  => ArrayMerge(name, arguments, obj)
      case _                              => GenericReversedFCall(name, arguments, obj)
    }
  }

}
