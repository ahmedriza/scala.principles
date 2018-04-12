package generics

class Vehicle

class Car extends Vehicle

class Container[+T] {

}

object Container {
  def apply[T](value: T) = new Container[T]()
}

class MyArray[+T] {

  // def udpate(x: T) = ??? // This is illegal.  covariant type T occurs in contravariant position in type T of value x
}

object Variance {

  def main(args: Array[String]): Unit = {
    val a: Container[Car] = Container(new Car)
    val b: Container[Vehicle] = a

    // To see why update() in MyArray[+T] is illegal:
    val intArray = new MyArray[Int]()
    // put some integers in intArray
    val objArray: MyArray[Any] = intArray
    // Now we can call udpate() with any type! even though the original was an int MyArray

  }

}
