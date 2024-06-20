package de.tubs.cs.ias.surfer

import wvlet.log.LogSupport

object implicits {
  implicit class OneableIterableOnce[T](l: IterableOnce[T]) {
    def one: T = {
      l.knownSize match {
        case -1 =>
          val iterator = l.iterator
          assert(iterator.hasNext, "expected one in IterableOnce, got 0")
          val tmp = iterator.next()
          assert(!iterator.hasNext, "expected one in IterableOnce, got more")
          tmp
        case x => assert(x == 1, "expected one in IterableOnce, got " + x + " (knownSize)")
          l.iterator.next()
      }
    }
  }

  implicit class UnwrappableCandidate[T](x: (List[Either[SSRFCandidate, Throwable]], Int, Int)) extends LogSupport {
    /**
      * get all SSRFCandidates from result. Throws the first exception if there is any. **Only intended for usage in tests!**
      *
      * @return
      */
    def unwrap: List[SSRFCandidate] = {
      val exceptions = x._1.collect { case Right(ex) => ex }
      exceptions match {
        case Nil => x._1.collect { case Left(v) => v }
        case _ => error(s"${exceptions.size} exceptions in results. throwing first one to scalatest.")
          throw exceptions.head
      }
    }
  }
}
