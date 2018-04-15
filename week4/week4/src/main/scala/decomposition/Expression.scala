package decomposition


trait Expr {
  def eval: Int = this match {
    case Number(n) => n
    case Sum(e1, e2) => e1.eval + e2.eval
  }
}

case class Number(n: Int) extends Expr

case class Sum(e1: Expr, e2: Expr) extends Expr

object Expression {

  def show(e: Expr): String = e match {
    case Number(n) => s"Number($n)"
    case Sum(e1, e2) => s"Sum(${show(e1)}, ${show(e1)})"
  }

  // Exercise (optional, harder)
  // Add case classes Var for variables x and Prod for products x * y
  // Change your show function so that it also deals with products.
  // Pay attention you get operator precedence right but to use as few parenthesis as possible
  //
  // Example
  // Sum(Prod(2, Var("x")), Var("y"))
  // should print as "2 * x + y".
  // But
  // Prod(Sum(2, Var("x")), Var("y"))
  // should print as "(2 + x) * y"

  def main(args: Array[String]): Unit = {
    val sum = Sum(Number(1), Number(2))
    println(show(sum))
    println(sum.eval)
  }
}
