import java.io.{File, PrintWriter}

import implicits.TupleImplicits._

import math._

object Part2 extends App {
  val function: (Double, Double) => Double = (x1: Double, x2: Double) => 100 * pow(x2 - x1 * x1, 2) + pow(1 - x1, 2)

  val counter = FunctionCounter(function.tupled)
  val countedFunction: (Double, Double) => Double = Function.untupled(counter.toFunction)

  val resultTable = Seq.tabulate(70)(i => pow(1.2, -i)).map { epsilon =>
    val descent = new GradientDescent2((-50, 50), countedFunction, epsilon)
    var stepCount = 1
    while (descent.makeAStepAndReturnDifference() > epsilon) {
      stepCount = stepCount + 1
    }

    (epsilon, stepCount, counter.currentCount, descent.position, function.tupled(descent.position))
  }

  val part2pw = new PrintWriter(new File("part2.csv"))
  part2pw.write(resultTable.toCsv("epsilon", "stepCount", "functionCallCount", "position", "function value").replace('.', ','))
  part2pw.close()

}
