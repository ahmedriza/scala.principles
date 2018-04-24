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
    ???
  }
}
