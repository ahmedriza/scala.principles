package org.onedigit.scala

object CombinatorialSearch {

  def main(args: Array[String]): Unit = {

  }

  def pairsPrime(n: Int): Seq[(Int, Int, Int)] = {

    def f(i: Int, n: Int): Seq[(Int, Int, Int)] =
      (1 until i).foldRight(List[(Int, Int, Int)]())((j, z) =>
        if (isPrime(i + j)) {
          (i, j, (i + j)) :: z
        } else {
          z
        })
    (1 until n).flatMap(i => {
      f(i, n)
    })
  }

  def pairsPrime2(n: Int): Seq[(Int, Int)] = {
    (1 until n) flatMap
      (i => (1 until i) map (j => (i, j))) filter (pair => isPrime(pair._1 + pair._2))
  }

  def pairsPrime3(n: Int): Seq[(Int, Int)] = {
    for {
      i <- 1 until n
      j <- 1 until i
      if (isPrime(i + j))
    } yield (i, j)
  }

  /**
   * @param n n
   * @return whether n is prime or not
   */
  def isPrime(n: Int): Boolean = {
    (2 until n).forall(i => n % i != 0)
  }

  // Exercise
  // Write a version of scalarProduct that makes use of a for expression
  def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
    (for ((a, b) <- xs zip ys) yield a * b).sum
  }
}
