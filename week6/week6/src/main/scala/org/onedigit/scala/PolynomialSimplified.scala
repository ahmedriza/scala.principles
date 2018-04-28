package org.onedigit.scala

/**
  * Scala maps are normally partial functions.  We can turn them into total functions by using
  * withDefaultValue
  */
class PolynomialSimplified(terms0: Map[Int, Double]) {

  def this(bindings: (Int, Double)*) = {
    this(bindings.toMap)
  }

  private val terms = terms0 withDefaultValue 0.0

  def +(other: PolynomialSimplified): PolynomialSimplified = {
    new PolynomialSimplified(terms ++ (other.terms map adjust))
  }

  def adjust(term: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }

  // Design another version of + in terms of foldLeft
  def add (other: PolynomialSimplified): PolynomialSimplified = {
    new PolynomialSimplified(
      other.terms.foldLeft(terms)(addTerm)
    )
  }

  def addTerm(z: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    z.+(exp -> (coeff + z(exp)))
  }

  override def toString: String = {
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp).mkString(" + ")
  }

}

object PolynomialSimplified {

  def main(args: Array[String]): Unit = {
    val p1 = new PolynomialSimplified(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
    val p2 = new PolynomialSimplified(0 -> 3.0, 3 -> 7.0)

    val p3 = p1 + p2
    println(p3)

    val p4 = p1.add(p2)
    println(p4)
  }
}
