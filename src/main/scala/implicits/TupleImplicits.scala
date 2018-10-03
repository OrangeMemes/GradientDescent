package implicits

object TupleImplicits {

  implicit class Tuple2Pimp(t: (Double, Double)) {
    def +(p: (Double, Double)): (Double, Double) = (p._1 + t._1, p._2 + t._2)

    def -(p: (Double, Double)): (Double, Double) = (p._1 - t._1, p._2 - t._2)

    def *(p: Double): (Double, Double) = (t._1 * p, t._2 * p)

    def /(p: Double): (Double, Double) = (t._1 / p, t._2 / p)

    def length: Double = Math.sqrt(t._1 * t._1 + t._2 * t._2)

    def unary_- : (Double, Double) = (-t._1, -t._2)
  }

  implicit class Tuple3Pimp(t: (Double, Double, Double)) {

    def +(p: (Double, Double, Double)): (Double, Double, Double) = (p._1 + t._1, p._2 + t._2, p._3 + t._3)

    def -(p: (Double, Double, Double)): (Double, Double, Double) = (p._1 - t._1, p._2 - t._2, p._3 - t._3)

    def *(p: Double): (Double, Double, Double) = (t._1 * p, t._2 * p, t._3 * p)

    def /(p: Double): (Double, Double, Double) = (t._1 / p, t._2 / p, t._3 / p)

    def mod: Double = Math.sqrt(t._1 * t._1 + t._2 * t._2 + t._3 * t._3)

    def unary_- : (Double, Double, Double) = (-t._1, -t._2, -t._3)
  }

  implicit class Tuple2SeqPimp[A, B](t: Seq[(A, B)]) {
    def toCsv(field1Name: String, field2Name: String): String =
      t.map(record => s"${record._1};${record._2}").mkString(
        start = s"$field1Name;$field2Name\n",
        sep = "\n",
        end = ""
      )
  }

  implicit class Tuple3SeqPimp[A, B, C](t: Seq[(A, B, C)]) {
    def toCsv(field1Name: String, field2Name: String, field3Name: String): String =
      t.map(record => s"${record._1};${record._2};${record._3}").mkString(
        start = s"$field1Name;$field2Name;$field3Name\n",
        sep = "\n",
        end = ""
      )
  }

  implicit class Tuple5SeqPimp[A, B, C, D, E](t: Seq[(A, B, C, D, E)]) {
    def toCsv(field1Name: String, field2Name: String, field3Name: String, field4Name: String, field5Name: String): String =
      t.map(record => s"${record._1};${record._2};${record._3};${record._4};${record._5}").mkString(
        start = s"$field1Name;$field2Name;$field3Name;$field4Name;$field5Name\n",
        sep = "\n",
        end = ""
      )
  }

}
