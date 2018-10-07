package implicits

import TupleImplicits._

import scala.annotation.tailrec

object FunctionImplicits {

  implicit class Function1Pimp(func: Double => Double) {
    def getMinInterval: (Double, Double) = getMinInterval(0, 1)

    @tailrec
    final def getMinInterval(x: Double, step: Double): (Double, Double) = {
      if (func(x) > func(x + step))
        getMinInterval(x + step, step * 2)
      else if (func(x) > func(x - step))
        getMinInterval(x - step, step * 2)
      else
        (x - step, x + step)
    }
  }

  implicit class Function2Pimp(func: (Double, Double) => Double) {
    def makeInVectorDirection(start: (Double, Double), vector: (Double, Double)): Double => Double =
      (lambda: Double) => func.tupled(start + (vector * lambda))
  }

  implicit class Function3Pimp(func: (Double, Double, Double) => Double) {
    def makeInVectorDirection(start: (Double, Double, Double), vector: (Double, Double, Double)): Double => Double =
      (lambda: Double) => func.tupled(start + (vector * lambda))
  }

}
