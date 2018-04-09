package idealized.scala

/**
  * This is one way to represent a Boolean object without using primitive boolean values.
  */
abstract class IdealBoolean {

  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => IdealBoolean): IdealBoolean = ifThenElse(x, False)
  def || (x: => IdealBoolean): IdealBoolean = ifThenElse(True, x)
  def unary_! : IdealBoolean = ifThenElse(False, True)

  def == (x: IdealBoolean): IdealBoolean = ifThenElse(x, !x)
  def != (x: IdealBoolean): IdealBoolean = ifThenElse(!x, x)

  // Assume False < True
  // --------------------
  // True < True   False
  // True < False  False
  // False < True  True
  // False < False False
  def < (x: IdealBoolean): IdealBoolean = ifThenElse(False, x)
}

object True extends IdealBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = t

  override def toString: String = "True"
}

object False extends IdealBoolean {
  override def ifThenElse[T](t: => T, e: => T): T = e

  override def toString: String = "False"
}

object IdealBoolean {

  def main(args: Array[String]): Unit = {
    println(True && False)
    println(!False)
    println(True == False)
    println(True != False)

    println("True  < True:  " + (True < True))
    println("True  < False: " + (True < False))
    println("False < True:  " + (False < True))
    println("False < False: " + (False < False))
  }
}