package idealized.scala

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

class Succ(n: Nat) extends Nat {

  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = {
    def loop(nat: Nat, acc: Nat): Nat = nat match {
      case Zero => acc
      case next => loop(next.predecessor, acc.successor)
    }
    loop(that, that)
  }

  override def -(that: Nat): Nat = ???

  override def toString: String = s"Succ($n)"
}

object Nat {
  def main(args: Array[String]): Unit = {
    val zero = Zero

    val one = zero.successor

    println(s"zero: $zero")
    println(s"one; $one")

    val two = one.successor
    val three = two.successor
    val four = one + three // three.successor

    println(s"two: $two")
    println(s"three: $three")
    println(s"four: $four")
  }
}