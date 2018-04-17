
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

    for (i <- 0 to list.length) {
      println(s"removeAt $i of $list: " + removeAt(i, list))
    }

    val list4 = List('a', 'b', 'c', 'd')
    for (i <- 0 to list.length) {
      println(s"removeAt $i of $list4: " + removeAt(i, list4))
    }

    val nested1 = List(List(1,1,10), 2)
    println("flattening: " + flatten(nested1))

    val nested2 = List(List(1,1), 2, List(3, List(5, 8)))
    println("flattening: " + flatten(nested2))
  }

  // -----------------------------------------------------------------

  /**
    * Flatten a list structure
    * List(List(1,1), 2, List(3, List(5, 8))) = List(1,1,2,3,5,8)
    */
  def flatten(xs: List[Any]): List[Any] = {
    xs match {
      case Nil                => Nil
      case (x: List[_]) :: ys => flatten(x) ::: flatten(ys)
      case y :: ys            =>  y :: flatten(ys)
    }
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

  def reverse[T](xs: List[T]): List[T] = {
    def loop(ys: List[T], acc: List[T]): List[T] = ys match {
      case Nil => acc
      case (z :: zs) => loop(zs, z :: acc)
    }
    loop(xs, Nil)
  }

  /**
    * Remove the nth element of a list xs. If n is out of bounds, return xs itself.
    */
  def removeAt[T](n: Int, xs: List[T]): List[T] = {

    def loop(count: Int, ys: List[T], acc: List[T]): List[T] = ys match {
      case Nil => ys
      case (z :: zs) =>
        if (count == n) {
          acc.reverse ++ zs
        } else {
          loop(count + 1, zs, z :: acc)
        }
    }

    loop(0, xs, Nil)
  }

  def removeAt_[T](n: Int, xs: List[T]): List[T] = {
    (xs take n) ::: (xs drop n+1)
  }

}
