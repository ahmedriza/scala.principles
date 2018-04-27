package org.onedigit.scala

/**
 * The eight queens problem is to place eight queens on a chessboard so
 * that no queen is threatened by another.
 *
 * In other words, there can't be two queens in the same row, column
 * or diagonal. Exmaple for a board of size 4x4:
 *
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
 * 
 *      0   1   2  3
 *    +---+-- +---+---+
 * 0  |   |   | x |   |
 *    +---+---+---+---+
 * 1  | x |   |   |   |
 *    +---+---+---+---+
 * 2  |   |   |   | x |
 *    +---+---+---+---+
 * 3  |   | x |   |   |
 *    +---+---+---+---+
 *
 *    (1, 3, 0, 2)
 *
 * We now develop a solution for a chessboard of any size, not just 8.
 *
 * One way to solve the problem is to place a queen on each row.
 *
 * Once we have placed k - 1 queens, one must place the kth queen in
 * a column where it's not "in check" with any other queen on the
 * board.
 *
 * We can solve this problem with a recursive algorithm:
 *
 * (1) Suppose that we have already generated all the solutions consisting
 *     of placing k-1 queens on a board of size n.
 *
 * (2) Each solution is represented by a list (of length k-1) containing
 *     the number of columns (between 0 and n-1).
 *
 * (3) The column number of the queen in the k-1 th row comes first in the
 *     list, followed by the column number of the queen in row k-2 etc.
 *
 * (4) The solution set is thus represented as a set of lists, with one
 *     element for each solution.
 *     Exmaple for the 4x4 board.  For the 3 queens placed, we would have the list
 *
 *     List(0, 3, 1)
 *
 *     Then we can add the fourth queen to get a solution:
 *
 *     List(2, 0, 3, 1)
 *
 * (5) Now, to place the kth queen, we generate all possible extensions
 *     of each solution preceded by a new queen
 *
 */

object Queens {
  def queens(n: Int): Set[List[Int]] = {
    println(s"queens, n = $n")
    val solution = findSolution(n, 0, 2, List[Int]())
    println(s"solution: $solution")
    Set()
  }

  def findSolution(n: Int, row: Int, col: Int, currentSolution: List[Int]): List[Int] = {
    if (currentSolution.length == n) { // found solution
      currentSolution
    } else if (col > n - 1) {
      println(s"Backtracking to row ${row - 1}, currentSolution is: $currentSolution")
      findSolution(n, row - 1, currentSolution.head + 1, currentSolution.tail)
    } else {
      println(s"row: $row, col: $col")
      if (isSafe(row, col, n, currentSolution)) {
        println(s"\tcol: $col is safe, current solution is: ${col :: currentSolution}")
        findSolution(n, row + 1, 0, col :: currentSolution)
      } else {
        findSolution(n, row, col + 1, currentSolution)
      }
    }
  }

  def isSafe(row: Int, col: Int, n: Int, solution: List[Int]): Boolean = {
    // Given a solution list, work out the rows and columns where the queens are located
    // The solution list contains the columns where a queen is. The first element in the list is for the last row
    val rowColumns = ((solution.length - 1) to 0 by -1) zip solution
    val result = for {
      d <- diagonals(row, col, n: Int)
      if (rowColumns.contains(d))
    } yield d

    !solution.contains(col) && result.isEmpty
  }

  def diagonals(row: Int, col: Int, n: Int): Seq[(Int, Int)] = {
    val ul = ((row - 1) to 0 by -1) zip ((col - 1) to 0 by -1)
    val ur = ((row - 1) to 0 by -1) zip ((col + 1) until n)
    val ll = ((row + 1) until n) zip ((col - 1) to 0 by -1)
    val lr = ((row + 1) until n) zip ((col + 1) until n)
    ul ++ ur ++ ll ++ lr
  }
}
