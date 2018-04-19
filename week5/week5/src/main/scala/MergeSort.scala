
object MergeSort {

  def main(args: Array[String]): Unit = {
    val a = List(1,1,2,5)
    val b = List(3,4,4,6)
    val merged = merge(a, b)
    println(merged)

    val c = List(10, 1, 5, 0, 1, 0, 2)
    println(sort(c))
    println(sort(List(2, -4, 5, 7, 1)))
  }

  def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, _) => ys
    case (_, Nil) => xs
    case (u :: us, v :: vs) =>
      if (u < v) u :: merge(us, ys)
      else v :: merge(xs, vs)
  }

  def merge__(left: List[Int], right: List[Int]): List[Int] = {
    left match {
      case Nil => right
      case (y :: ys) =>
        right match {
          case Nil => left
          case (z :: zs) =>
            if (y <= z) {
              y :: merge__(ys, right)
            } else {
              z :: merge__(left, zs)
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
