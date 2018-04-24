

object Reduction {

  def main(args: Array[String]): Unit = {

    val list1 = List(1, 2, 3, 4, 5)
    val s1 = list1 reduceLeft (_ + _)
    println(s1)

    println("foldRight: " + list1.foldRight(0)(_ + _))

    // println(::(2, ::(10, Nil)))

    val list2 = List(7, 8)
    println("concat: " + concat(list1, list2))

    println("mapFun: " + mapFun(list1)(_ * 2))
    println("lengthFun: " + lengthFun(list1))
  }

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case (y :: ys) => y + sum(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] =  xs.foldRight(ys)(_ :: _)

  // Use foldRight to implement the following
  def mapFun[T, U](xs: List[T])(f: T => U): List[U] =
    (xs foldRight List[U]())( (a, b) => f(a) :: b )

  def lengthFun[T](xs: List[T]): Int = (xs foldRight 0)((_, b) => b+1)

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
