package idealized.scala

import scala.annotation.tailrec

abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new IllegalArgumentException("Zero has no predecessor")
  override def successor: Nat = new Succ(Zero)
  override def +(that: Nat): Nat = that
  override def -(that: Nat): Nat = throw new IllegalArgumentException("Cannot subtract from zero")
  override def toString: String = "Zero"
}

class Succ(val n: Nat) extends Nat {

  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = {
    @tailrec
    def loop(nat: Nat, acc: Nat): Nat = {
      if (nat == Zero) {
        acc
      } else {
        loop(nat.predecessor, acc.successor)
      }
    }
    loop(that, this)
  }

  override def -(that: Nat): Nat = {
    def loop(nat: Nat, acc: Nat): Nat = {
      if (nat == Zero) {
        acc
      } else {
        loop(nat.predecessor, acc.predecessor)
      }
    }
    loop(that, this)
  }

  override def toString: String = s"Succ($n)"

  override def equals(obj: scala.Any): Boolean = obj match {
    case s: Succ => s.n == this.n
    case _ => false
  }
}

object Nat {
  def main(args: Array[String]): Unit = {
    val zero = Zero

    val one = zero.successor

    println(s"zero: $zero")
    println(s"one; $one")

    val two = one.successor
    val three = two.successor
    val four = two + two // three.successor
    val five = three + two
    val seven = four + three

    println(s"two: $two")
    println(s"three: $three")
    println(s"four: $four")
    println(s"five: $five")
    println(s"seven: $seven")

  }
}