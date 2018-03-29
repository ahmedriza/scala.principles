package funsets

import scala.annotation.tailrec


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = x => x == elem


  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union(s: Set, t: Set): Set = x =>  contains(s, x) || contains(t, x)

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect(s: Set, t: Set): Set = x => contains(s, x) && contains(t, x)

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)


  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 10

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (contains(s,a) && !p(a)) false
      else if (a > bound) true
      else iter(a+1)
    }

    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    *
    * To obtain this in terms of forall, we use the following derivation
    * (i) We note that the negation of exists involves forall:
    *     not (exists x in A, p(x)) <=> forall x in A, not p(x)
    * (ii) What we want is exists in terms of forall, so negate the above
    *     not (not (exists x in A, p(x))) <=> not (forall x in A, not p(x))
    */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map(s: Set, f: Int => Int): Set = {
    x => {
      val fx = f(x)
      println(s"x = $x, fx = $fx")
      s(fx)
    }
  }

  /**
    * Displays the contents of a set
    */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }

}
