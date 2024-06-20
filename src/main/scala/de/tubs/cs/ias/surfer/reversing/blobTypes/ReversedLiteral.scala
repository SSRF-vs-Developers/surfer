package de.tubs.cs.ias.surfer.reversing.blobTypes

import de.tubs.cs.ias.surfer.reversing.blobTypes.LiteralTypes.{BOOLEAN, LiteralType, NUMBER, STRING}
import de.tubs.cs.ias.surfer.reversing.{ReversedBlob, ReversingCache}

/** This implementation of a ReversedBlob represents a constant String, i.e., Literal
  *
  * @author Simon Koch
  *
  * @param value the value the constant string can take
  */
case class ReversedLiteral(value: String) extends ReversedBlob {

  override val children: List[ReversedBlob] = List()

  def getLiteralType: LiteralType = {
    try {
      value.toFloat
      NUMBER
    } catch {
      case _: Throwable =>
        if (Set("true", "false").contains(value.toLowerCase)) {
          BOOLEAN
        } else {
          STRING
        }
    }
  }

  override def getBlobStringRepresentationInternal(
      implicit cache: ReversingCache): String = value

}
