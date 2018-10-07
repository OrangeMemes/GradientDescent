

case class FunctionCounter[T1, T2](f: T1 => T2) {
  val counter = new SimpleCounter

  val toFunction: T1 => T2 = { x: T1 =>
    counter.inc()
    f(x)
  }

  def currentCount: Int = counter.currentCount

  def reset(): Unit = {
    counter.reset()
  }
}
