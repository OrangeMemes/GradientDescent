import derivative.SimpleDerivativeMaker
import implicits.FunctionImplicits._
import implicits.TupleImplicits._
import minimum_methods.GoldenMinimumMethod

class GradientDescent2(start: (Double, Double), f: (Double, Double) => Double, val epsilon: Double) {
  private var currentPosition: (Double, Double) = start
  private val derivativeMaker = SimpleDerivativeMaker(epsilon / 1E2)

  def makeAStepAndReturnDifference(): Double = {
    val gradientValue = derivativeMaker.gradient(f).tupled(currentPosition)
    val direction: (Double, Double) = -gradientValue / gradientValue.length
    val vectorDirectedF = f.makeInVectorDirection(currentPosition, direction)
    val (a, b) = vectorDirectedF.getMinInterval

    val minimumMethod = new GoldenMinimumMethod(a, b, vectorDirectedF)
    while (minimumMethod.currentSpread > epsilon / 10)
      minimumMethod.makeAStep()

    val prevPosition = currentPosition
    currentPosition = currentPosition + direction * minimumMethod.result

    (prevPosition - currentPosition).length
  }

  def position: (Double, Double) = currentPosition


}
