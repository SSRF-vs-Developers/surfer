package de.tubs.cs.ias.surfer

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class LRUCacheTest extends AnyWordSpec with Matchers {
  "simple usage" in {
    def f(x: Int): Int = x * x

    val cache = new LRUCache[Int, Int](3)
    cache.get(0, f) shouldBe f(0)
    cache.misses shouldBe 1

    cache.get(0, f) shouldBe f(0)
    cache.misses shouldBe 1
    cache.hits shouldBe 1

    cache.get(1, f) shouldBe f(1)
    cache.misses shouldBe 2
    cache.hits shouldBe 1

    cache.get(1, f) shouldBe f(1)
    cache.misses shouldBe 2
    cache.hits shouldBe 2

    cache.get(2, f) shouldBe f(2)
    cache.misses shouldBe 3
    cache.hits shouldBe 2

    cache.get(3, f) shouldBe f(3)
    cache.misses shouldBe 4
    cache.hits shouldBe 2

    cache.get(0, f) shouldBe f(0)
    cache.misses shouldBe 5
    cache.hits shouldBe 2

  }
}
