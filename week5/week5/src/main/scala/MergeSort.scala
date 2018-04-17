
object MergeSort {

  def main(args: Array[String]): Unit = {
    val a = List(1,2,5)
    val b = List(3,4,6)
    val merged = merge(a, b)
    println(merged)

    val c = List(10, 1, 5, 0, 2)
    println(sort(c))
  }

  def merge(left: List[Int], right: List[Int]): List[Int] = {
    left match {
      case Nil => right
      case (y :: ys) =>
        right match {
          case Nil => left
          case (z :: zs) =>
            if (y <= z) {
              y :: z :: merge(ys, zs)
            } else {
              z :: y :: merge(ys, zs)
            }
        }
    }
  }

  def sort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) {
      xs
    } else {
      val (fst, snd) = xs splitAt n
      // sort first and second halves and merge them
      merge(sort(fst), sort(snd))
    }
  }
}
