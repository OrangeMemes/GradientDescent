package minimum_methods

abstract class AbstractMinimumMethod(a0: Double, b0: Double, val inspectedFunction: Double => Double) {
  protected var a: Double = a0
  protected var b: Double = b0

  var f1 = inspectedFunction(getNewX1)
  var f2 = inspectedFunction(getNewX2)

  def makeAStep(): Unit = {

    if (f1 < f2)
      b = x2
    else
      a = x1

    val (oldX1, oldX2) = (x1, x2)

    (getNewX1, getNewX2) match {
      case (`oldX2`, newX2) =>
        f1 = f2
        x1Var = x2

        f2 = inspectedFunction(newX2)
        x2Var = newX2
      case (`oldX1`, newX2) =>
        f2 = inspectedFunction(newX2)
        x2Var = newX2
      case (newX1, `oldX1`) =>
        f2 = f1
        x2Var = x1

        f1 = inspectedFunction(newX1)
        x1Var = newX1
      case (newX1, `oldX2`) =>
        f1 = inspectedFunction(newX1)
        x1Var = newX1
      case (newX1, newX2) =>
        x1Var = newX1
        f1 = inspectedFunction(newX1)

        x2Var = newX2
        f2 = inspectedFunction(newX2)
    }
  }

  def currentSpread: Double = b - a

  def result: Double = (a + b) / 2

  protected def getNewX1: Double

  protected def getNewX2: Double

  private var x1Var: Double = getNewX1

  private var x2Var: Double = getNewX2

  final def x1: Double = x1Var

  final def x2: Double = x2Var
}
