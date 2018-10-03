import scala.collection.mutable

case class CachedAndCountedFunction[T1, T2](f: T1 => T2) {
  val counter = new SimpleCounter
  val cache: mutable.HashMap[T1, T2] = mutable.HashMap[T1, T2]()

  def getFunc: T1 => T2 = { x: T1 =>
    if (!cache.contains(x)) {
      counter.inc()
      cache.put(x, f(x))
    }
    cache(x)
  }

  def currentCount: Int = counter.currentCount
  def reset(): Unit = {
    counter.reset()
    cache.clear()
  }
}
