package week3

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

trait MyList[T] {
  def isEmpty: Boolean
  def head: T
  def tail: MyList[T]
}

class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {
  override def isEmpty: Boolean = false

  override def toString: String = {
    s"$head, $tail"
  }
}

class MyNil[T] extends MyList[T] {
  override def isEmpty: Boolean = true
  override def head: Nothing = throw new NoSuchElementException("Nil has no head")
  override def tail: Nothing = throw new NoSuchElementException("Nil has no tail")

  override def toString: String = "Nil"
}

object ListObject {

  def singleton[T](elem: T) = new Cons(elem, new MyNil)

  /**
    * nth that takes an integer n and a list and selects the n'th element of the list.
    * Elements are number from 0.  If index is outside the range from 0 up to the length of the list minus one,
    * an IndexOutOfBoundsException should be thrown.
    * @param index index
    * @param list list
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

  def main(args: Array[String]): Unit = {
    val list = new Cons(30, new Cons(20, new Cons(10, new MyNil)))
    println(list)
    println(nth(2, list))
  }
}
