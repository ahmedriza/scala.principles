object Collections {

  def main(args: Array[String]): Unit = {

    val vec = Vector[Int](50, 60, 70)
    val vec2: Vector[Int] = 10 +: vec
    println(vec)
    println(vec2)
    println("vec2(0): " + vec2.apply(0))

    val xs: Array[Int] = Array(1, 2, 3, 44)
    println("sum = " + xs.sum)
    println("max = " + xs.max)

    combinations()

    val v1 = Vector(1.0,2.0,3.0)
    val v2 = Vector(10.0, 20.0, 30.0)
    println("scala product: " + scalaProduct(v1, v2))

    for (i <- 1 to 100) {
      if(isPrime(i))
        println(s"isPrime $i: " + isPrime(i))
    }
  }

  // List all combinations of numbers x and y where x is drawn
  // from 1..M and y is drawn from 1..N
  def combinations(): Unit = {
    val M: Int = 2
    val N: Int = 3

    def f(x: Int): Seq[(Int, Int)] = (1 to N) map (y => (x, y))
    val result = (1 to M) flatMap (x =>  f(x)) // (1 to N) map (y => (x, y)))
    println(result)
  }

  /**
    *  Scala product 
    *  @param xs xs
    *  @param ys ys
    */
  def scalaProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    val result = (xs zip ys) map {
      case (x, y) => x  * y
    } sum
  
    result
  }

  /**
    *  A number is prime if the only divisors of n are 1 and n itself.
    */
  def isPrime(n: Int): Boolean = {
    (2 until n) forall (d => n % d != 0)
  }
}
