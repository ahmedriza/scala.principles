package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._

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

    println(comb1)
    println(comb2)

    val expected =
      List(Fork(Leaf('g',1),Leaf('h',1),List('g', 'h'),2), Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2))
    assert(comb2 === expected)
  }

  test("combine of some leaf list 2") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4), Leaf('y', 5))

    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4), Leaf('y', 5)))
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

    println(result)

    val expected = List(
      Fork(
        Fork(
          Fork(Leaf('c',1),Leaf('d',1),List('c', 'd'),2),
          Fork(
            Fork(Leaf('g',1),Leaf('h',1),List('g', 'h'),2),
            Fork(Leaf('e',1),Leaf('f',1),List('e', 'f'),2),
            List('g', 'h', 'e', 'f'),4
          ),List('c', 'd', 'g', 'h', 'e', 'f'),6
        ),
        Fork(Leaf('a',8),Leaf('b',3),List('a', 'b'),11),
        List('c', 'd', 'g', 'h', 'e', 'f', 'a', 'b'),17
      )
    )


    val expected_ =
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


  // ---

  test("mergeCodeTables on a, empty b") {
    val a: CodeTable = List(('a', List(1)), ('b', List(0)))
    val b: CodeTable = List()
    val m1 = mergeCodeTables(a, b)
    assert(m1 === List(('a', List(1)), ('b', List(0))))
  }

  test("mergeCodeTables on empty a, b") {
    val a: CodeTable = List()
    val b: CodeTable = List(('a', List(1)), ('b', List(0)))
    val m1 = mergeCodeTables(a, b)
    assert(m1 === List(('a', List(1)), ('b', List(0))))
  }


  test("mergeCodeTables on disjoint a, b, b larger that a") {
    val a: CodeTable = List(('a', List(0)))
    val b: CodeTable = List(('b', List(1)), ('d', List(0)))

    val m1 = mergeCodeTables(a, b)
    m1 should contain theSameElementsAs List(('a', List(0)), ('b', List(1)), ('d', List(0)))
  }

  test("mergeCodeTables on disjoint a, b, a larger that b") {
    val a: CodeTable = List(('b', List(1)), ('d', List(0)))
    val b: CodeTable = List(('a', List(0)))

    val m1 = mergeCodeTables(a, b)
    m1 should contain theSameElementsAs List(('a', List(0)), ('b', List(1)), ('d', List(0)))
  }

  test("mergeCodeTables on disjoint a, b, a same size as b") {
    val a: CodeTable = List(('b', List(1)), ('d', List(0)))
    val b: CodeTable = List(('a', List(0)), ('c', List(1)))

    val m1 = mergeCodeTables(a, b)
    m1 should contain theSameElementsAs List(('a', List(0)), ('b', List(1)), ('d', List(0)), ('c', List(1)))
  }

  test("mergeCodeTables on a, b with one element in common") {
    val a: CodeTable = List(('a', List(0)), ('b', List(1)), ('c', List(0)))
    val b: CodeTable = List(('a', List(1)), ('d', List(0)))
    val c: CodeTable = List(('a', List(1)), ('b', List(0)))

    val m1 = mergeCodeTables(a, b)
    m1 should contain theSameElementsAs List(('a', List(0, 1)), ('b', List(1)), ('d', List(0)), ('c', List(0)))

    val m2 = mergeCodeTables(m1, c)
    m2 should contain theSameElementsAs List(('a', List(0, 1, 1)), ('b', List(1, 0)), ('d', List(0)), ('c', List(0)))
  }

  test("mergeCodeTables on a, b with last two elements in common") {
    val a: CodeTable = List(('e', List(0)), ('f', List(0)), ('g', List(0)), ('h', List(0)))
    val b: CodeTable = List(('g', List(1)), ('h', List(1)))

    val m1 = mergeCodeTables(a, b)
    m1 should contain theSameElementsAs List(('e', List(0)), ('f', List(0)), ('g', List(0, 1)), ('h', List(0, 1)))
  }

  test("mergeCodeTables on a, b with second element in common") {
    // This case should not happen in practise
    val a: CodeTable = List(('z', List(0, 1)), ('d', List(0)), ('b', List(1)), ('c', List(0)))
    val b: CodeTable = List(('a', List(1)), ('b', List(0)))

    val m1 = mergeCodeTables(a, b)

    // m1 should contain theSameElementsAs List(('a', List(0, 1, 1)), ('b', List(1, 0)), ('d', List(0)), ('c', List(0)))
  }

  test("convert on one node") {
    val codeTree = createCodeTree(List('e', 'f', 'g', 'h'))
    val codeTable = convert(codeTree)
    codeTable should contain theSameElementsAs List(
      ('e',List(1, 1)),
      ('f',List(1, 0)),
      ('g',List(0, 1)),
      ('h',List(0, 0))
    )
  }

  test("loop on small tree") {
    val codeTree = createCodeTree(repeat('a', 8) ++
      repeat('b', 3) ++
      repeat('c', 1) ++
      repeat('d', 1) ++
      repeat('e', 1) ++
      repeat('f', 1) ++
      repeat('g', 1) ++
      repeat('h', 1)
    )

    val codeTable = convert(codeTree)

    codeTable should contain theSameElementsAs List(
      ('a',List(1, 1, 1)),
      ('b',List(1, 1, 0)),
      ('c',List(1, 0, 1)),
      ('d',List(1, 0, 0)),
      ('e',List(0, 1, 1)),
      ('f',List(0, 1, 0)),
      ('g',List(0, 0, 1)),
      ('h',List(0, 0, 0))
    )
  }

  test("codeBits should find bits for a given char") {
    val codeTree = createCodeTree(repeat('a', 8) ++
      repeat('b', 3) ++
      repeat('c', 1) ++
      repeat('d', 1) ++
      repeat('e', 1) ++
      repeat('f', 1) ++
      repeat('g', 1) ++
      repeat('h', 1)
    )

    val codeTable = convert(codeTree)
    assert(codeBits(codeTable)('a') === List(1, 1, 1))
    assert(codeBits(codeTable)('b') === List(1, 1, 0))
    assert(codeBits(codeTable)('c') === List(1, 0, 1))
    assert(codeBits(codeTable)('d') === List(1, 0, 0))
    assert(codeBits(codeTable)('e') === List(0, 1, 1))
    assert(codeBits(codeTable)('f') === List(0, 1, 0))
    assert(codeBits(codeTable)('g') === List(0, 0, 1))
    assert(codeBits(codeTable)('h') === List(0, 0, 0))
  }

  test("quickEncode on secret") {
    val secretCode = List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')
    val bits = quickEncode(frenchCode)(secretCode)
    assert(bits === secret)
  }

  // ---------

  private def repeat(c: Char, n: Int): List[Char] = List.fill(n)(c)
}
