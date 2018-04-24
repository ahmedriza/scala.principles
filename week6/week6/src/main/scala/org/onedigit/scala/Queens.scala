package org.onedigit.scala

/**
  * The eight queens problem is to place eight queens on a chessboard so 
  * that no queen is threatened by another.
  * 
  * In other words, there can't be two queens in the same row, column 
  * or diagonal.
  * 
  * We now develop a solution for a chessboard of any size, not just 8.
  * 
  * One way to solve the problem is to place a queen on each row.
  * 
  * Once we have placed k - 1 queens, one must place the kth queen in 
  * a column where it's not "in check" with any other queen on the 
  * board.
  */  
