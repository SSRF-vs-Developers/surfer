package de.tubs.cs.ias.surfer.reversing

import de.tubs.cs.ias.surfer.util.{Cache, NoCache}
import wvlet.log.LogSupport

class ReversingCache(verbose: Boolean, context: String) extends LogSupport {

  protected val reversingCache
    : Cache[(BytecodeReversingRule, Long, Int), List[ReversedBlob]] = new Cache(
    "ReversingCache")
  protected val fullConditionCache: Cache[ReversedBlob, ReversedBlob] =
    new Cache("FullConditionCache", verbose = verbose, context = context)
  protected val blobStringRepresentationCache: Cache[ReversedBlob, String] =
    new Cache("BlobStringRepresentationCache")

  override def toString: String = {
    s"$reversingCache // $fullConditionCache // $blobStringRepresentationCache"
  }

  def clear(): Unit = {
    reversingCache.clear()
    fullConditionCache.clear()
    blobStringRepresentationCache.clear()
    debug("full cache flush")
  }
  def withBlobStringRepresentationcache[T](
      lambda: Cache[ReversedBlob, String] => T): T = {
    lambda(getBlobStringRepresentationCache)
  }

  def getBlobStringRepresentationCache: Cache[ReversedBlob, String] =
    blobStringRepresentationCache

  def withReversingCache[T](lambda: Cache[(BytecodeReversingRule, Long, Int),
                                          List[ReversedBlob]] => T): T = {
    lambda(getReversingCache)
  }

  def getReversingCache
    : Cache[(BytecodeReversingRule, Long, Int), List[ReversedBlob]] =
    reversingCache

}

class ReversingNoCache extends ReversingCache(false, "") {

  override protected val fullConditionCache: Cache[ReversedBlob, ReversedBlob] =
    new NoCache()
  override protected val blobStringRepresentationCache
    : Cache[ReversedBlob, String] = new NoCache()

}
