package minimum_methods

class DichotomyMinimumMethod(delta: Double, a0: Double, b0: Double, f: Double => Double)
  extends AbstractMinimumMethod(a0, b0, f) {

  override def getNewX1: Double = (a + b) / 2 - delta

  override def getNewX2: Double = (a + b) / 2 + delta
}
