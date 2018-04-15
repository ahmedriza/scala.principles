package Lists

object Exercise {

  def main(args: Array[String]): Unit = {

    val list = List(1, 2, List(10, 20), 30)
    ex1(list)

    val list2 = List(7, 3, 9, 2)
    println(isort(list2))
  }

  // Consider the pattern x :: y :: List(xs, ys) :: zs.
  // What is the condition that describes most accurately the length L of the list it matches
  def ex1(list: List[Any]) = list match {
    case x :: y :: List(xs, ys) :: zs =>
      println(s"zs: $zs")
    case _ => println("no match")
  }

  // Suppose we want to sort a list of numbers in ascending order:
  // One way to sort the list List(7, 3, 9, 2) is to sort the tail
  // List(3, 9, 2) to obtain List(2, 3, 9)

  // The next step is to insert the head 7 in the right place to obtain the result
  // List(2, 3, 7, 9)

  // This idea describes Insertion Sort:

  def isort(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => insert(y, isort(ys))
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case Nil => List(x)
    case (y :: ys) =>
      if (x > y) y :: insert(x,  ys)
      else x :: xs
  }
}
