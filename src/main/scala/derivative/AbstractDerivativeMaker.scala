package derivative

trait AbstractDerivativeMaker {
  def derivative(f: Double => Double): Double => Double

  def derivative(f: (Double, Double) => Double): (Double, Double) => Double =
    (x: Double, y: Double) => derivative(f(x, _))(y) + derivative(f(_, y))(x)

  def derivative(f: (Double, Double, Double) => Double): (Double, Double, Double) => Double =
    (x: Double, y: Double, z: Double) => derivative(f(x, y, _))(z) + derivative(f(x, _, z))(y) + derivative(f(_, y, z))(x)

  def gradient(f: (Double, Double) => Double): (Double, Double) => (Double, Double) =
    (x: Double, y: Double) => (derivative(f(_, y))(x), derivative(f(x, _))(y))

  def gradient(f: (Double, Double, Double) => Double): (Double, Double, Double) => (Double, Double, Double) =
    (x: Double, y: Double, z: Double) => (derivative(f(_, y, z))(x), derivative(f(x, _, z))(y), derivative(f(x, y, _))(z))
}
