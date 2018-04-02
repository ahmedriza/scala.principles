package week3

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.List

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
  def intersect(other: IntSet): IntSet
  def flatten(): List[Int]
  def fromList(list: List[Int]): IntSet
}

// Consider implementing sets as binary trees.
// There are two possible types of trees:
//   a tree for the empty set
//   a tree consisting of an integer and two sub trees
//
//            6
//           / \
//          /   \
//         /     \
//        5      11
//      /  \    /  \
//     E   E   7    12
//            / \   / \
//           E  8  E   E
//             / \
//            E  9
//              / \
//             E  E

/**
  * Singleton week3.Empty object (only one instance exists).
  */
class Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  override def union(other: IntSet): IntSet = other
  override def intersect(other: IntSet): IntSet = new Empty
  override def toString: String = s"E"
  override def flatten(): List[Int] = List()

  override def fromList(list: List[Int]): IntSet = new Empty
}

class NonEmpty(val elem: Int, val left: IntSet, val right: IntSet) extends IntSet {

  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  /**
    * Note that this is purely functional and the tree is persistent, i.e. we do not modify the previous tree
    * when adding new nodes.
    *
    * For example, let's say the tree so far is this:
    *            6
    *           / \
    *          /   \
    *         /     \
    *        5      11
    *      /  \    /  \
    *     E   E   E    E
    *
    * If we now call incl(7), here is what happens
    *
    *             6    new tree
    *           /  \
    *          /    \
    *         /      \
    *         |      11
    *         |     /  \
    *         |    7    E
    *         |   / \
    *         |  E   E      6  old tree
    *         |            / \
    *         |           /   \
    *         |          /     \
    *         ------->  5      11
    *                 /  \    /  \
    *                E   E   E    E
    *
    * Note how the old tree is lef unmodified, and the new tree points to the left subtree (starting with node 5) of
    * the old one, since that branch was not modified and both old and new trees share that same branch.
    */

  override def incl(x: Int): IntSet = {
    if (x < elem) { // include x in left sub-tree
      new NonEmpty(elem, left incl x, right)
    } else if (x > elem) { // include x in right sub-tree
      new NonEmpty(elem, left, right incl x)
    } else {
      this
    }
  }

  override def toString: String = s"{$left $elem $right}"

  def union_(other: IntSet): IntSet = {
    ((left union right) union other) incl elem
  }

  //
  //   4
  //  / \
  // E   5
  //    / \
  //   E   E

  def union(other: IntSet): IntSet = {

    def loop(set: IntSet, other: IntSet): IntSet = {
      set match {
        case _: Empty => other
        case e: NonEmpty =>
          val afterLeft = loop(e.left, other.incl(e.elem))
          loop(e.right, afterLeft)
      }
    }

    loop(other, this)

    // fromList(other.flatten())
  }

  def flatten(): List[Int] = {

    def loop(set: IntSet, buffer: ListBuffer[Int]): Unit = {
      set match {
        case e: Empty => ()
        case e: NonEmpty =>
          buffer += e.elem
          loop(e.left, buffer)
          loop(e.right, buffer)
      }
    }

    val buffer = ListBuffer[Int]()
    loop(this, buffer)
    buffer.toList
  }

  override def fromList(list: List[Int]): IntSet = {
    @tailrec
    def loop(xs: List[Int], acc: IntSet): IntSet = {
      xs match {
        case Nil => acc
        case h :: t => loop(t, acc.incl(h))
      }
    }
    loop(list, this)
  }

  override def intersect(other: IntSet): IntSet = {
    def loop(set: IntSet, result: IntSet): IntSet = {
      set match {
        case _: Empty => result
        case e: NonEmpty =>
          val afterLeft = loop(e.left, result)
          val afterRight = loop(e.right, afterLeft)
          if (other.contains(e.elem)) {
            afterRight.incl(e.elem)
          } else {
            afterRight
          }
      }
    }
    loop(this, new Empty)
  }
}

object IntSet {

  def main(args: Array[String]): Unit = {

    val s1 = new Empty().incl(6)
    val s2 = s1.incl(5)
    val s3 = s2.incl(11)
    val s4 = s3.incl(7)
    val s5 = s4.incl(12)
    val s6 = s5.incl(8)
    val s7 = s6.incl(9)

    val s8 = new NonEmpty(4, new Empty, new Empty)
    val s9 = s8.incl(5)

    // val s9 = s7.union(s8)
    // println(s9)

    println(s"s7 = $s7")
    println(s"s9 = $s9")
    val s10 = s7 union s9
    println(s"s7 union s9 = $s10")

    val s7_list = s7.flatten()
    println(s7_list)
    val s9_list = s9.flatten()
    println(s9_list)

    println(s"s7 = ${s7.fromList(s9_list)}")

  }
}