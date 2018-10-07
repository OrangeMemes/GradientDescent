package minimum_methods

import scala.collection.mutable.ArrayBuffer

object Fibonacci {

  private var cache: ArrayBuffer[Double] = ArrayBuffer(1, 1)

  def apply(i: Int): Double = {
    while (cache.length < i)
      cache = cache :+ cache.takeRight(2).sum

    cache(i - 1)
  }
}
