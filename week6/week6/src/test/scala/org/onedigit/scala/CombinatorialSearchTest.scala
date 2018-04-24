package org.onedigit.scala

import org.scalatest.FunSuite

class CombinatorialSearchTest extends FunSuite {

  test("pairs sum prime") {
    val result1 = CombinatorialSearch.pairsPrime(7)
    assert(result1 === List((2,1,3), (3,2,5), (4,1,5), (4,3,7), (5,2,7), (6,1,7), (6,5,11)))

    val result2 = CombinatorialSearch.pairsPrime2(7)
    assert(result2 === List((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5)))

    val result3 = CombinatorialSearch.pairsPrime3(7)
    assert(result3 === List((2,1), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5)))

    val list1 = List(1.0, 2.0, 3.0)
    // 1 + 4 + 9 = 14
    val dp = CombinatorialSearch.scalarProduct(list1, list1)
    assert(dp === 14)
  }
}
