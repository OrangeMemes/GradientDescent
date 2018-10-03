package minimum_methods

abstract class AbstractMinimumMethod(a0: Double, b0: Double, val inspectedFunction: Double => Double) {
  protected var a: Double = a0
  protected var b: Double = b0

  def x1: Double

  def x2: Double

  def makeAStep(): Unit = {
    val f1 = inspectedFunction(x1)
    val f2 = inspectedFunction(x2)

    if (f1 < f2)
      b = x2
    else if (f1 > f2)
      a = x1
    else {
      a = x1
      b = x2
    }
  }

  def currentSpread: Double = b - a

  def result: Double = (a + b) / 2
}
