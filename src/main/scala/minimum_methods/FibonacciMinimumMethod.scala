package minimum_methods

class FibonacciMinimumMethod(epsilon: Double, val a0: Double, val b0: Double, f: Double => Double)
  extends AbstractMinimumMethod(a0, b0, f) {

  private var stepNum: Int = 0

  override def makeAStep(): Unit = {
    assert(stepNum < n)
    stepNum = stepNum + 1
    super.makeAStep()
  }

  private lazy val n: Int = {
    val floor = (b - a) / epsilon
    var i = 1
    while (Fibonacci(i) <= floor)
      i = i + 1
    i
  }

  protected def getNewX1: Double = {
    a + Fibonacci(n - stepNum) / Fibonacci(n + 2) * (b0 - a0)
  }


  protected def getNewX2: Double = {
    a + Fibonacci(n - stepNum + 1) / Fibonacci(n + 2) * (b0 - a0)
  }
}
