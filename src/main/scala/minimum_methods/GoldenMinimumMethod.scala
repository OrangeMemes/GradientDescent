package minimum_methods

import scala.math._

class GoldenMinimumMethod(a0: Double, b0: Double, f: Double => Double)
  extends AbstractMinimumMethod(a0, b0, f) {


  override def makeAStep(): Unit = {
    val f1 = inspectedFunction(x1Var)
    val f2 = inspectedFunction(x2Var)

    if (f1 < f2) {
      b = x2Var
      x2Var = x1Var
      x1Var = a + (3 - sqrt(5)) / 2 * (b - a)
    } else if (f1 > f2) {
      a = x1Var
      x1Var = x2Var
      x2Var = a + (sqrt(5) - 1) / 2 * (b - a)
    } else {
      a = x1Var
      b = x2Var
      x1Var = a + (3 - sqrt(5)) / 2 * (b - a)
      x2Var = a + (sqrt(5) - 1) / 2 * (b - a)
    }
  }

  var x1Var: Double = a + (3 - sqrt(5)) / 2 * (b - a)

  var x2Var: Double = a + (sqrt(5) - 1) / 2 * (b - a)

  override def x1: Double = x1Var

  override def x2: Double = x2Var
}
