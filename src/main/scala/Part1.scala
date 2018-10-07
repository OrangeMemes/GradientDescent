import java.io.{File, PrintWriter}

import minimum_methods.{DichotomyMinimumMethod, FibonacciMinimumMethod, GoldenMinimumMethod}

import scala.math._
import implicits.TupleImplicits._

object Part1 extends App {

  val counter = FunctionCounter(sin)

  val f = counter.toFunction

  val dichPW = new PrintWriter(new File("dichotomy.csv"))

  val a: Double = -Pi / 2
  val b: Double = Pi / 2

  val dichotomyResultTable = Seq.tabulate(100)(i => pow(1.25, -i)).map { epsilon =>
    counter.reset()

    val dichotomyMinimumMethod = new DichotomyMinimumMethod(epsilon / 4, a, b, f)
    while (dichotomyMinimumMethod.currentSpread > epsilon)
      dichotomyMinimumMethod.makeAStep()

    (epsilon, counter.currentCount, dichotomyMinimumMethod.result)
  }

  dichPW.write(dichotomyResultTable.toCsv("epsilon", "count", "result").replace('.', ','))
  dichPW.close()

  val goldPW = new PrintWriter(new File("golden.csv"))

  val goldenResultTable = Seq.tabulate(100)(i => pow(1.25, -i)).map { epsilon =>
    counter.reset()

    val goldenMinimumMethod = new GoldenMinimumMethod(a, b, f)
    while (goldenMinimumMethod.currentSpread > epsilon)
      goldenMinimumMethod.makeAStep()

    (epsilon, counter.currentCount, goldenMinimumMethod.result)
  }

  goldPW.write(goldenResultTable.toCsv("epsilon", "count", "result").replace('.', ','))
  goldPW.close()

  val fibPW = new PrintWriter(new File("fibonacci.csv"))

  val fibonacciResultTable = Seq.tabulate(100)(i => pow(1.25, -i)).map { epsilon =>
    counter.reset()

    val fibonacciMinimumMethod = new FibonacciMinimumMethod(epsilon, a, b, f)
    while (fibonacciMinimumMethod.currentSpread > epsilon)
      fibonacciMinimumMethod.makeAStep()

    (epsilon, counter.currentCount, fibonacciMinimumMethod.result)
  }

  fibPW.write(fibonacciResultTable.toCsv("epsilon", "count", "result").replace('.', ','))
  fibPW.close()

}
