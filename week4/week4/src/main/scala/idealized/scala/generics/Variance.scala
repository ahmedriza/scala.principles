package idealized.scala.generics

class Vehicle

class Car extends Vehicle

class Container[+T] {

}

object Container {
  def apply[T](value: T) = new Container[T]()
}

object Variance {

  def main(args: Array[String]): Unit = {

    val a: Container[Car] = Container(new Car)

    val b: Container[Vehicle] = a
  }

}
