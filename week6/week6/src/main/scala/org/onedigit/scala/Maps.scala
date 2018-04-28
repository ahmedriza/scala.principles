package org.onedigit.scala

object Maps {

  def main(args: Array[String]): Unit = {

    // Sorting
    val fruits = List("apple", "pear", "orange", "pineapple")
    val sorted = fruits.sortWith{case (a,b) => a < b}
    println(sorted)

    val grouped: Map[Char, List[String]] = fruits.groupBy(k => k.head)
    println(grouped)

  }

}
