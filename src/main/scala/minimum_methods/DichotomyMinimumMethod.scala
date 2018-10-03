package minimum_methods

class DichotomyMinimumMethod(delta: Double, a0: Double, b0: Double, f: Double => Double)
  extends AbstractMinimumMethod(a0, b0, f) {

  override def x1: Double = (a + b) / 2 - delta

  override def x2: Double = (a + b) / 2 + delta
}
