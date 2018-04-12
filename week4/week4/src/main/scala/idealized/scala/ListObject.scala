package idealized.scala

import scala.annotation.tailrec


/**
  * A list is either:
  *
  * <ul>
  *   <li>an empty list, new Nil, or</li>
  *   <li>a list, new Const(x, xs) consisting of a head element x and a tail list xs.
  * </ul>
  *
  * List(1, 2, 3)
  *
  * <pre>
  * -----
  * | | |
  * -----
  *  1   \
  *      -----
  *      | | |
  *      -----
  *      2    \
  *           -----
  *           | | |
  *           -----
  *           3    \
  *                 Nil
  * </pre>
  *
  * List(List(1, 2), List(3))
  *
  * <pre>
  *              -----
  *              | | |
  *              -----
  *             /     \
  *            /       \
  *           /         -----
  *      -----          | | |
  *      | | |          -----
  *      -----           \   \
  *     /     \           \   Nil
  *    1       -----       \
  *            | | |        -----
  *            -----        | | |
  *           /     \       -----
  *          2     Nil     /     \
  *                       3      Nil
  * </pre>
  */

trait MyList[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]
  def prepend[U >: T] (elem: U): MyList[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    s"$head, $tail"
  }
}

object MyNil extends MyList[Nothing] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil has no head")
  override def tail: Nothing = throw new NoSuchElementException("Nil has no tail")

  override def toString: String = "Nil"
}

object MyList {

  def singleton[T](elem: T) = new Cons(elem, MyNil)

  def apply[T](): MyList[T] = MyNil

  def apply(n: Int): MyList[Int] = new Cons[Int](n, MyNil)

  def apply(n1: Int, n2: Int): MyList[Int] = new Cons[Int](n1, new Cons(n2, MyNil))

  /**
    * nth that takes an integer n and a list and selects the n'th element of the list.
    * Elements are number from 0.  If index is outside the range from 0 up to the length of the list minus one,
    * an IndexOutOfBoundsException should be thrown.
    *
    * @param index index
    * @param list  list
    * @return value at index
    * @throws IndexOutOfBoundsException if the index is out of bounds
    */
  def nth[T](index: Int, list: MyList[T]): T = {
    if (list.isEmpty) throw new IndexOutOfBoundsException
    if (index == 0) {
      list.head
    } else {
      nth(index - 1, list.tail)
    }
  }
}

class Car

object ListObject {
  def main(args: Array[String]): Unit = {
    val list = new Cons(30, new Cons(20, new Cons(10, MyNil)))
    println("nth: " + MyList.nth(2, list))

    val list1 = MyList()
    val list2 = MyList(10)
    val list3 = MyList(10, 20)

    val list4: MyList[Any] = list3.prepend(new Car)

    println(list1)
    println(list2)
    println(list3)
    println(list4)
  }
}
