
abstract class IntSet {
  def contains(x: Int): Boolean
  def incl(x: Int): IntSet
}

// Consider implementing sets as binary trees.
// There are two possible types of trees:
//   a tree for the empty set
//   a tree consisting of an integer and two sub trees
//
//            10
//           / \
            /   \
//         /     \
//        5       7
//      /  \    /  \
//     E   E   6    12
//            / \   / \
//           E  E  E   E
//
class Empty extends IntSet {
  override def contains(x: Int): Boolean = false
  override def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  override def incl(x: Int): IntSet = {
    if (x < elem) { // include x in left sub-tree
      new NonEmpty(elem, left incl x, right)
    } else if (x > elem) { // include x in right sub-tree
      new NonEmpty(elem, left, right incl x)
    } else {
      this
    }
  }
}