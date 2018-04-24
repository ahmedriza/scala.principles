package org.onedigit.scala

object Collections {

  def main(args: Array[String]): Unit = {
  }

  // List all combinations of numbers x and y where x is drawn
  // from 1..M and y is drawn from 1..N
  def combinations(): Unit = {
    val M: Int = 2
    val N: Int = 3

    def f(x: Int): Seq[(Int, Int)] = (1 to N) map (y => (x, y))
    val result = (1 to M) flatMap (x => f(x)) // (1 to N) map (y => (x, y)))
  }

  /**
   *  Scala product
   *  @param xs xs
   *  @param ys ys
   */
  def scalaProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    (xs.zip(ys)).map {
      case (x, y) => x * y
    }.sum
  }

  /**
   *  A number is prime if the only divisors of n are 1 and n itself.
   */
  def isPrime(n: Int): Boolean = {
    (2 until n) forall (d => n % d != 0)
  }
}
