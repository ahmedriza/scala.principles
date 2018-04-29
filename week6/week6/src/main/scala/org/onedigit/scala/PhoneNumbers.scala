package org.onedigit.scala

import scala.io.{BufferedSource, Source}

class PhoneNumbers {

  import PhoneNumbers._

  // Assume you're given a dictionary, words, as a list of words.
  // Design a method, translate, such that
  // translate(phoneNumber)
  // produces all phrases of words that can serve as mnemonics for the
  // phone number

  // Exmaple, the number "7225247386" should have mnemonic "Scala is fun"
  // as one element of the set of solutions.

  // Invert the mnemonic map to give a map from 'A' ... 'Z' to '2' ... '9'

  // val charCode: Map[Char, Char] =

  val charCode: Map[Char, Char] = mnemonics.foldLeft(Map[Char, Char]())(charCodeMapper)

  private def charCodeMapper(map: Map[Char, Char], kv: (Char, String)): Map[Char, Char] = {
    val (key, mnemoic) = kv
    map ++ mnemoic.map(c => c -> key).toMap
  }

  /**
    * Maps a word to the digit string it can represent, e.g. "Java"  -> "5282"
    * @param word
    * @return
    */
  def wordCode(word: String): String = ???

  /**
    * A map from digit strings to words that reprsent them,
    * e.g. Java -> List("Java", "Kata", "Lava", ...)
    * Note that a missing number should map to the empty set, e.g. "1111" -> List()
    */

}

object PhoneNumbers {

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

  val in: BufferedSource = Source.fromFile("linuxwords.txt")

  def main(args: Array[String]): Unit = {
    val words = in.getLines()

    val phoneNumbers = new PhoneNumbers
    println(phoneNumbers.charCode)

    val tmp = "Java".map(c => phoneNumbers.charCode.get(c.toUpper))
    println(tmp)
  }

}
