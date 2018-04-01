package week3


class Node(private val x: Int, private val msg: String) {

  def this(x: Int) = this(x, x.toString)

  def < (that: Node): Boolean = this.x < that.x

  def > (that: Node): Boolean = this.x > that.x

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case that: Node => this.x == that.x
      case _ => false
    }
  }

  override def hashCode(): Int = 41 * (41 + this.x)

  override def toString: String = s"$x"
}

abstract class NodeSet {
  def contains(x: Node): Boolean
  def incl(x: Node): NodeSet
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

class Leaf extends NodeSet {
  override def contains(x: Node): Boolean = false
  override def incl(x: Node): NodeSet = new Element(x, new Leaf, new Leaf)

  // override def toString: String = s"E"
}

class Element(node: Node, left: NodeSet, right: NodeSet) extends NodeSet {

  println(s"Constructing week3.Element with node: $node, left: $left, right: $right")

  override def contains(x: Node): Boolean = {
    if (x < node) left contains x
    else if (x > node) right contains x
    else true
  }

  override def incl(x: Node): NodeSet = {
    if (x < node) { // include x in left sub-tree
      new Element(node, left incl x, right)
    } else if (x > node) { // include x in right sub-tree
      new Element(node, left, right incl x)
    } else {
      this
    }
  }

  // override def toString: String = s"$elem, left: $left, right: $right"
}

object NodeSet {
  def main(args: Array[String]): Unit = {
    val head = new Leaf().incl(new Node(6))
    val n1 = head.incl(new Node(5))
    val n2 = n1.incl(new Node(11))

    val n3 = n2.incl(new Node(7))

    val n4 = n3.incl(new Node(12))
    val n5 = n4.incl(new Node(8))
    val n6 = n5.incl(new Node(9))

    val n7 = n6.incl(new Node(7))
    println()
  }
}