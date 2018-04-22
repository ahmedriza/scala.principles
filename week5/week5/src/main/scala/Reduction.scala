

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

  def product(xs: List[Int]): Int = xs match {
    case Nil => 1
    case (y :: ys) => y * product(ys)
  }

  def foldLeft[T, U](xs: List[T], z: U)(op: (U, T) => U): U = xs match {
    case Nil => z
    case (y :: ys) => foldLeft(ys, op(z, y))(op)
  }

  def foldRight[T, U](xs: List[T], z: U)(op: (T, U) => U): U = xs match {
    case Nil => z
    case (y :: ys) => op(y, foldRight(ys, z)(op))
  }
}
