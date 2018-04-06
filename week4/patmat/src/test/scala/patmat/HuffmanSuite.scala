package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine of some leaf lists and forks") {
    val e = Leaf('e', 1)
    val f = Leaf('f', 1)
    val g = Leaf('g', 1)
    val h = Leaf('h', 1)

    val leafList = List(e, f, g, h)
    val comb1 = combine(leafList)
    val comb2 = combine(comb1)

    val expected = List(Fork(
      Fork(Leaf('e',1), Leaf('f',1), List('e', 'f'), 2),
      Fork(Leaf('g',1), Leaf('h',1), List('g', 'h'), 2),
      List('e', 'f', 'g', 'h'),
      4)
    )

    assert(comb2 === expected)
  }

  test("until on list of code trees") {
    val a = Leaf('a', 8)
    val b = Leaf('b', 3)
    val c = Leaf('c', 1)
    val d = Leaf('d', 1)
    val e = Leaf('e', 1)
    val f = Leaf('f', 1)
    val g = Leaf('g', 1)
    val h = Leaf('h', 1)

    val leafList = List(a, b, c, d, e, f, g, h)
    val result = until(singleton, combine)(leafList)

    val expected =
      List(
        Fork(
          Fork(
            Fork(Leaf('a',8),Leaf('b',3),List('a', 'b'),11),
            Fork(Leaf('c',1),Leaf('d',1),List('c', 'd'),2),
            List('a', 'b', 'c', 'd'),13),
          Fork(
            Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),
            Fork(Leaf('g',1),Leaf('h',1),List('g', 'h'),2),
            List('e', 'f', 'g', 'h'),4),
          List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),17))

    assert(result === expected)
  }

  test("createCodeTree from some text") {
    val codeTree = createCodeTree(
      repeat('a', 8) ++
        repeat('b', 3) ++
        repeat('c', 1) ++
        repeat('d', 1) ++
        repeat('e', 1) ++
        repeat('f', 1) ++
        repeat('g', 1) ++
        repeat('h', 1)
    )

    val expected =
      Fork(
        Fork(
          Fork(Leaf('h',1), Leaf('g',1), List('h', 'g'),2),
          Fork(Leaf('f',1), Leaf('e',1), List('f', 'e'),2),
          List('h', 'g', 'f', 'e'),4),
        Fork(
          Fork(Leaf('d',1), Leaf('c',1), List('d', 'c'),2),
          Fork(Leaf('b',3), Leaf('a',8), List('b', 'a'),11),
          List('d', 'c', 'b', 'a'),13),
        List('h', 'g', 'f', 'e', 'd', 'c', 'b', 'a'),17)

    //                     (hgfedcba) 17
    //                 /                 \
    //          (hgfe) 4                   (dcba) 13
    //       /           \               /         \
    //   (hg) 2          (fe) 2         (dc) 2      (ba) 11
    // /      \          /     \       /    \      /    \
    // h(1)   g(1)     f(1)    e(1)   d(1)   c(1) b(3)  a(8)

    assert(codeTree === expected)
  }

  test("decode of a simple tree") {
    val codeTree = createCodeTree(
      repeat('a', 8) ++
        repeat('b', 3) ++
        repeat('c', 1) ++
        repeat('d', 1) ++
        repeat('e', 1) ++
        repeat('f', 1) ++
        repeat('g', 1) ++
        repeat('h', 1)
    )

    val result = decode(codeTree, List(1,0,0,0,1,0,1,0,1))
    assert(result === List('d', 'f', 'c'))
  }

  test("encode of a simple text") {
    val codeTree = createCodeTree(
      repeat('a', 8) ++
        repeat('b', 3) ++
        repeat('c', 1) ++
        repeat('d', 1) ++
        repeat('e', 1) ++
        repeat('f', 1) ++
      repeat('g', 1) ++
        repeat('h', 1)
    )

    val result = encode(codeTree)(List('d', 'f', 'c'))
    assert(result === List(1,0,0,0,1,0,1,0,1))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("loop on small tree") {
    val codeTree = createCodeTree(
      repeat('a', 8) ++
        repeat('b', 3) ++
        repeat('c', 1) ++
        repeat('d', 1) ++
        repeat('e', 1) ++
        repeat('f', 1) ++
        repeat('g', 1) ++
        repeat('h', 1)
    )

    val result = loop(codeTree, List())
    println(result)
  }

  // ---------

  private def repeat(c: Char, n: Int): List[Char] = List.fill(n)(c)
}
