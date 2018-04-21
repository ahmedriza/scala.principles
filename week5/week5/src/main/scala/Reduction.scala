

object Reduction {

  def main(args: Array[String]): Unit = {

    val list1 = List(1, 2, 3, 4, 5)
    val s1 = list1 reduceLeft (_ + _)
    println(s1)
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case (y :: ys) => y + sum(ys)
  }
}
