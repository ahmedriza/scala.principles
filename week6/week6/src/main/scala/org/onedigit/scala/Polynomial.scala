package org.onedigit.scala

import scala.collection.immutable.TreeMap

// A polynomial can be seen as a map from exponents to coefficients
// For example:
//   x^3 - 2x + 5
// can be represented with the map.
// Map (0 -> 5, 1 -> 2, 3 -> 1)

class Polynomial(val terms: TreeMap[Int, Double]) {

  def + (other: Polynomial): Polynomial = {

    val keys = terms.keys ++ other.terms.keys

    val result = keys.foldLeft(TreeMap[Int, Double]()) {
      case (map, key) =>
        val thisCoeff: Double = terms.getOrElse(key, 0)
        val thatCoeff: Double = other.terms.getOrElse(key, 0)
        val sum: Double = thisCoeff + thatCoeff
        map.+(key -> sum)
    }

    new Polynomial(result)
  }

  // Another way to define addition
  def add(other: Polynomial): Polynomial = {

    def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      terms get exp match {
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None         => exp -> coeff
      }
    }

    new Polynomial(terms ++ (other.terms map adjust))
  }

  override def toString: String = {
    (for ((exp, coeff) <- terms.toList.reverse) yield coeff + "x^" + exp).mkString(" + ")
  }
}

object Polynomial  {
  def main(args: Array[String]): Unit = {

    val p1 = new Polynomial(TreeMap(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Polynomial(TreeMap(0 -> 3.0, 3 -> 7.0))

    val p3 = p1 + p2
    println(p3)

    val p4 = p1.add(p2)
    println(p4)
  }
}
