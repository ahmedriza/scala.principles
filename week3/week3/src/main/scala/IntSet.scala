

abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
  def union(other: IntSet): IntSet
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
  * Singleton Empty object (only one instance exists).
  */
object Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def union(other: IntSet): IntSet = other

  override def toString: String = s"E"
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

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

  override def union(other: IntSet): IntSet = {
    ???
  }
}

object IntSet {

  def main(args: Array[String]): Unit = {

    val head = Empty.incl(6)
    val n1 = head.incl(5)
    val n2 = n1.incl(11)
    val n3 = n2.incl(7)
    val n4 = n3.incl(12)
    val n5 = n4.incl(8)
    val n6 = n5.incl(9)

    val n7 = n6.incl(7)

    println(n7)
  }
}