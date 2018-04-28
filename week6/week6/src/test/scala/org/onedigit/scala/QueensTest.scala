package org.onedigit.scala

class QueensTest extends org.scalatest.FunSuite {

  test("diag") {
    // diagonal cells of (5,3) on an 8x8 board
    val result = Queens.diagonals(5, 3, 8)
    assert(result === Vector((4, 2), (3, 1), (2, 0), (4, 4), (3, 5), (2, 6), (1, 7), (6, 2), (7, 1), (6, 4), (7, 5)))
  }

  /**
   *      0   1   2  3
   *    +---+-- +---+---+
   * 0  |   | x |   |   |
   *    +---+---+---+---+
   * 1  |   |   |   | x |
   *    +---+---+---+---+
   * 2  | x |   |   |   |
   *    +---+---+---+---+
   * 3  |   |   | x |   |
   *    +---+---+---+---+
   *
   *    (2, 0, 3, 1)
   */
  test("isSafe_") {
    // Vector((2, 0), (1, 3), (0, 1))
    val solution = List(0, 3, 1)
    // diagonal for (3, 0): Vector((2,1), (1,2), (0,3))
    // diagonal for (3, 2): Vector((2,1), (1,0), (2,3))
    assert(Queens.isSafe(3, 0, 4, solution) === false)
    assert(Queens.isSafe(3, 1, 4, solution) === false)
    assert(Queens.isSafe(3, 2, 4, solution) === true)
    assert(Queens.isSafe(3, 3, 4, solution) === false)
  }

  test("isSafe") {
    val queens = List(0, 3, 1)

    assert(Queens.isSafe(0, queens) === false)
    assert(Queens.isSafe(1, queens) === false)
    assert(Queens.isSafe(2, queens) === true)
    assert(Queens.isSafe(3, queens) === false)
  }

  /**
   *      0   1   2  3
   *    +---+-- +---+---+
   * 0  | x |   |   |   |
   *    +---+---+---+---+
   * 1  |   |   | x |   |
   *    +---+---+---+---+
   * 2  |   |   |   |   |
   *    +---+---+---+---+
   * 3  |   |   |   |   |
   *    +---+---+---+---+
   */
  test("queens") {
    val solutions = Queens.queens(4)
    println("Number of solutions: " + solutions.length)
    solutions.foreach(println)
  }

  test("filter") {

    val list1 = (0 to 10).withFilter(p => p % 2 == 0).map(x => x)
    println(list1)

    val list2 = (0 to 10).filter(p => {
      p % 2 == 0
    })
    println(list2)
  }
}
