package de.tubs.cs.ias.surfer.util

import wvlet.log.LogSupport

import scala.collection.mutable.{Map => MMap}

class Cache[T, X](name: String = "Generic Cache",
                  reporting: Boolean = false,
                  verbose: Boolean = false,
                  context: String = "")
    extends LogSupport {

  private var hits: Int = 0
  private var misses: Int = 0
  private val REPORTING_THRESHOLD = 100

  private val cache: MMap[T, X] = MMap()

  override def toString: String = s"$cache"

  def get(key: T): Option[X] = synchronized {
    if (reporting) {
      if (cache.contains(key)) {
        hits = hits + 1
      } else {
        misses = misses + 1
      }
      if (((hits + misses) % REPORTING_THRESHOLD) == 0) {
        info(
          s"$name -> hits: $hits // misses : $misses // size : ${cache.size}")
      }
    }
    val ret = cache.get(key)
    ret
  }

  def add(pair: (T, X)): X = synchronized {
    if (verbose) {
      //info(s"$context adding $pair")
      //if(pair._1.isInstanceOf[ReversedFCall] && pair._1.asInstanceOf[ReversedFCall].name == "curl_init") {
      //  throw new RuntimeException("this is were the shit happens")
      //}
    }
    cache.addOne(pair)
    pair._2
  }

  def clear(): Unit = synchronized {
    if (reporting) {
      info(s"$name -> hits: $hits // misses : $misses // size : ${cache.size}")
    }
    cache.clear()
  }

}

class NoCache[T, X]() extends Cache[T, X] {

  override def get(key: T): Option[X] = None

  override def add(pair: (T, X)): X = pair._2

  override def clear(): Unit = {}

}
