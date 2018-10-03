package minimum_methods

import scala.math._

class FibonacciMinimumMethod(epsilon: Double, val a0: Double, val b0: Double, f: Double => Double)
  extends AbstractMinimumMethod(a0, b0, f) {

  private var stepNum: Int = 1

  override def makeAStep(): Unit = {
    super.makeAStep()
    stepNum = stepNum + 1
  }

  private val n: Int = {
    val floor = (b - a) / epsilon
    var i = 1
    while (fibonacci(i) <= floor)
      i = i + 1

    i - 2
  }



  private def fibonacci(n: Int): Double = {
    1 / sqrt(5) * (pow((1 + sqrt(5)) / 2, n) - pow((1 - sqrt(5)) / 2, n))
  }

  override def x1: Double = a + fibonacci(n - stepNum + 1) / fibonacci(n + 2) * (b0 - a0)

  override def x2: Double = a + fibonacci(n - stepNum + 2) / fibonacci(n + 2) * (b0 - a0)
}
