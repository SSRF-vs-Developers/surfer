package de.tubs.cs.ias.surfer

import scala.collection.mutable

// adapted after https://blog.knoldus.com/what-is-lru-cache-and-how-to-implement-it-in-scala/
class LRUCache[K, V](val capacity: Int = 1000) {
  private val hashMap = mutable.LinkedHashMap.empty[K, V]

  var hits = 0
  var misses = 0

  def get(key: K, expensive_func: K => V): V = {
    if (hashMap.contains(key)) {
      val value = hashMap.remove(key).get
      hashMap.addOne(key -> value)
      hits += 1
      value
    } else {
      misses += 1
      val value = expensive_func(key)
      if (hashMap.size == capacity) {
        hashMap.remove(hashMap.head._1)
        hashMap.addOne(key -> value)
      } else {
        hashMap.addOne(key -> value)
      }
      value
    }
  }
}
