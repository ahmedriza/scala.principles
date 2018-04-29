package org.onedigit.scala

import scala.collection.immutable
import scala.io.{BufferedSource, Source}

// Assume you're given a dictionary, words, as a list of words.
// Design a method, translate, such that
// translate(phoneNumber)
// produces all phrases of words that can serve as mnemonics for the
// phone number

// Example, the number "7225247386" should have mnemonic "Scala is fun"
// as one element of the set of solutions.

object PhoneNumbers {

  val in: BufferedSource = Source.fromFile("linuxwords.txt")

  val words: List[String] = in.getLines().toList.filter(word => word.forall(chr => chr.isLetter))

  // Phone key mnemonics
  val mnemonics = Map(
    '2' -> "ABC",
    '3' -> "DEF",
    '4' -> "GHI",
    '5' -> "JKL",
    '6' -> "MNO",
    '7' -> "PQRS",
    '8' -> "TUV",
    '9' -> "WXYZ",
  )

  // Invert the mnemonic map to give a map from 'A' ... 'Z' to '2' ... '9'
  val charCode: Map[Char, Char] = mnemonics.foldLeft(Map[Char, Char]())(charCodeMapper)

  private def charCodeMapper(map: Map[Char, Char], kv: (Char, String)): Map[Char, Char] = {
    val (key, mnemoic) = kv
    map ++ mnemoic.map(c => c -> key).toMap
  }

  // Alternative implementation of charCode
  val charCode_ : Map[Char, Char] = for ( (digit, str) <- mnemonics; ltr <- str) yield ltr -> digit

  /**
    * Maps a word to the digit string it can represent, e.g. "Java"  -> "5282"
    */
  def wordCode(word: String): String = word.toUpperCase.map(charCode)

  /**
    * A map from digit strings to words that represent them,
    * e.g. "5282" -> List("Java", "Kata", "Lava", ...)
    * Note that a missing number should map to the empty set, e.g. "1111" -> List()
    */
  def wordsForNum: Map[String, List[String]] = words.groupBy(wordCode) withDefaultValue Nil

  /**
    * Return all the ways to encode a number as a list of words.
    * e.g. encode("7225247386")
    */
  def encode(number: String): Set[List[String]] = {
    ???
  }

  def recursiveEncode(number: String, acc: List[String]): Unit = {
    if (number.isEmpty) {
      println(acc)
    } else {
      val words: List[String] = wordsForNum(number)
    }
  }


  def main(args: Array[String]): Unit = {

    println(wordsForNum.get("7225"))
    println(wordsForNum.get("247"))
    println(wordsForNum.get("386"))
  }

}
