
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

trait List[T] {

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {

}

class Nil[T] extends List[T]

