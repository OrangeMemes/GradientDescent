package derivative

case class SimpleDerivativeMaker(step: Double) extends AbstractDerivativeMaker {
  override def derivative(f: Double => Double): Double => Double =
    (x: Double) => (f(x + step) - f(x)) / step
}
