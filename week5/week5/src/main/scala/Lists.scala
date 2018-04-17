
object Lists {

  def main(args: Array[String]): Unit = {
    val list = List(10, 20, 30, 40)
    println("init: " + init(list))
    println("last: " + last(list))
    val list2 = List(50, 60)
    println(concat(list, list2))
    println(concat(Nil, List(1, 2)))
    println(reverse(list))

    val list3 = Nil
    println(list3.reverse)
  }

  def last[T](xs: List[T]): T = xs match {
    case Nil => throw new Error("last of empty list")
    case List(x) => x
    case (y :: ys) => last(ys)
  }

  def init[T](xs: List[T]): List[T] = xs match {
    case Nil => throw new Error("init of empty list")
    case List(x) => Nil
    case (y :: ys) => y :: init(ys)
  }

  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case Nil => ys
    case (x :: xxs) => x :: concat(xxs, ys)
  }

  def reverse[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case y :: ys => reverse(ys) ++ List(y)
  }
}
