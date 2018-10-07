package minimum_methods

import scala.math._

class GoldenMinimumMethod(a0: Double, b0: Double, f: Double => Double)
  extends AbstractMinimumMethod(a0, b0, f) {



  override def makeAStep(): Unit = {

    if (f1 < f2) {
      b = x2Var

      x2Var = x1Var
      f2 = f1

      x1Var = getNewX1
      f1 = inspectedFunction(x1Var)
    } else {
      a = x1Var

      x1Var = x2Var
      f1 = f2

      x2Var = getNewX2
      f2 = inspectedFunction(x2Var)
    }
  }

  var x1Var: Double = getNewX1

  var x2Var: Double = getNewX2

  protected def getNewX1: Double = {
    a + (3 - sqrt(5)) / 2 * (b - a)
  }

  protected def getNewX2: Double = {
    a + (sqrt(5) - 1) / 2 * (b - a)
  }

}
