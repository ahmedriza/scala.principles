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

  val in: BufferedSource = Source.fromFile("src/main/resources/linuxwords.txt")

  val words: List[String] = in.getLines().toList.filter(word => word.forall(chr => chr.isLetter))

  // Phone key mnemonics
  val mnemonics: Map[Char, String] = Map(
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
  val wordsForNum: Map[String, List[String]] = words.groupBy(wordCode) withDefaultValue Nil

  /**
    * Return all the ways to encode a number as a list of words.
    * e.g. encode("7225247386")
    */
  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) {
      Set(List())
    } else {

      val result = (1 to number.length).flatMap(i => {
        val prefix = number.take(i)
        val w = wordsForNum(prefix)
        // println(s"i: $i, number: $number, prefix: $prefix, w: $w")

        w.flatMap(word => {
          val suffix = number.drop(i)
          val r = encode(suffix)
          println(s"r: $r")
          r.map(
            rest => {
              // println(s"\ti: $i, rest: $rest")
              word :: rest
            })
        })
      })

      /*
        val result = for {
          split <- 1 to num.length
          word <- wordsForNum(num.take(split))
          rest <- encode(num.drop(split))

        } yield word :: rest
        */
      val set = result.toSet
      println(s"number: $number, result: $set")
      set
    }
  }

  // if we have solution 7, then add all solutions for 225..."
  // 7225 24 73 86
  //
  // 7225 ->
  //     247386...
  // 72252
  //     47386...
  // 722524
  //     7386...
  // 7225247
  //     386...
  // 72252473
  //     86...
  // 722524738
  //     6...
  // 7225247386
  //

  // 247386
  //
  // 274
  //    386...
  // 2743
  //    86...
  // 27438
  //    6...
  // 274386
  //

  def recursiveEncode(number: String, acc: List[String]): Unit = {
    if (number.isEmpty) {
      println(acc)
    } else {
      for (i <- 1 to number.length) {
        val word = wordsForNum(number take i)
        recursiveEncode(number drop i, word ++ acc)
      }
    }
  }


  def main(args: Array[String]): Unit = {
    // val number = "7225247386"
    val number = "47386"
    // println(wordsForNum.get("72252"))
    // println(wordsForNum.get("473"))
    // println(wordsForNum.get("86"))
    // recursiveEncode(number, List())
    encode(number).foreach(println)
  }

}
