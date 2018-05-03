package forcomp


object Anagrams {

  def main(args: Array[String]): Unit = {
    println(dictionary.length)

    val str = "Ahmeda"
    println(wordOccurrences(str))

    val list = List("ahmed", "foo", "bar")
    println(sentenceOccurrences(list))

    println(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))))
    println(wordAnagrams("learn"))

    println("-------------------------------------")
    val occurrences: Occurrences = List(('a', 2), ('b', 2))
    val expected = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 1)),
      List(('a', 2), ('b', 2))
    )
    println(s"Combinations of $occurrences")
    combinations(occurrences).foreach(println)
    println("-------------------------------------")

    combs(occurrences) foreach println

    println("-------------------------------------")
    val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val y = List(('r', 1))
    val xMap = x.toMap
    val yMap = y.toMap

    val tmp = xMap.foldLeft(xMap) {
      case (map, (c, i)) =>
        yMap.get(c) match {
          case None => map
          case Some(yValue) =>
            val newValue = i - yValue
            if (newValue == 0) {
              map.-(c)
            } else {
              map.updated(c, i - yValue)
            }
        }
    }

    println("-------------------------------------")

    sentenceAnagrams(List("Yes", "man"))
    // sentenceAnagrams(List("en", "as", "my"))
    // sentenceAnagrams(List("my", "sane"))
  }

  // -----------

  def combinations_(chars: Occurrences): List[Occurrences] = chars match {
    case Nil => List(Nil)
    case (c, freq) :: tail =>
      val subPowerset = combinations(tail)
      subPowerset.foldLeft(subPowerset){case (acc, str) =>
        (1 to freq).foldLeft(acc){case (acc2, freq2) => (c -> freq2 :: str) :: acc2}
      }
  }

  // -----------

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /** `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /** Converts the word into its character occurrence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   *
   *  Note: you must use `groupBy` to implement this method!
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase
    .groupBy(c => c)
    .map {  case (ch, lst) => (ch, lst.length)  }
    .toList
    .sortBy { case (ch, _) => ch }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

  /** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences)

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =  dictionaryByOccurrences.getOrElse(wordOccurrences(word), Nil)

  /** Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */

  private val e1: List[Occurrences] = List(List())

  private def expand(acc: List[Occurrences], occurrence: (Char, Int)): List[Occurrences] = occurrence match {
    case (c, int) =>
      (for {
        i <- 0 to int
        ao <- acc
      } yield if (i == 0) ao else {(c, i) :: ao}).toList
  }

  /**
    * How can we find all possible combinations of (("a", 2), ("b", 1), ("c", 1))?
    * First, we need to reduce the problem to a smaller-size problem and solve it recursively.
    * All combinations of (("b", 1), ("c", 1)) are:
    * (), (("b", 1)), (("c", 1)), (("b", 1), ("c", 1)).
    * Then, we can add to each of these combinations zero, one or two "a"s
    * (since we have two "a"s originally). So, all combinations in which we are interested are:
    *
    * (), (("a", 1)), (("a", 2)),
    * (), (("b", 1)), (("c", 1)), (("b", 1), ("c", 1)).
    * (("b", 1)), (("a", 1), ("b", 1)), (("a", 2), ("b", 1)),
    *
    * (("c", 1)), (("a", 1), ("c", 1)), (("a", 2), ("c", 1)),
    * (("b", 1), ("c", 1)), (("a", 1), ("b", 1), ("c", 1)), (("a", 2), ("b", 1), ("c", 1))
    *
    * Example (2)
    * ------------
    * pairs of first <- (), ((a,1)), ((a,2))
    * rest           <- (), ((b,1)), ((b,2))
    *
    * Take each pair of first and add to rest:
    *
    * ((a,1), (b,1)), ((a,2), (b,1)), ((a,1), (b,2)), ((a,2), (b,2))
    *
    */

  def combs(list: List[(Char, Int)]): List[List[(Char, Int)]] = list match {
    case Nil => List(Nil)
    case x :: xs =>
      val rest = combs(xs)
      // generate all possible pairs of x
      val expansion = (for {
        xp <- pairs(x)
      } yield addPair(xp, rest)).flatten

      expansion ++ rest
  }

  // generate all possible pairs from x.
  // e.g. if x = ('a', 2), generate ('a', 1), ('a', 2)
  def pairs(x: (Char, Int)): List[(Char, Int)] = {
    (for {
      i <- 1 to x._2
    } yield (x._1, i)).toList
  }

  // add x to each element in rest
  def addPair(x: (Char, Int), rest: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
    for {
      r <- rest
    } yield x :: r
  }

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    // occurrences.reverse.foldLeft(e1)(expand)
    combs(occurrences)
  }

  /** Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   * For example, given two occurrence lists for words `lard` and `r`:
   * val x = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
   * val y = List(('r', 1))
   *
   * the `subtract(x, y)` is `List(('a', 1), ('d', 1), ('l', 1))`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   *
   *  Hint: you can use `foldLeft`, and `-`, `apply` and `updated` operations on `Map`.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val xMap = x.toMap
    val yMap = y.toMap

    xMap.foldLeft(xMap) {
      case (map, (c, i)) =>
        yMap.get(c) match {
          case None => map
          case Some(yValue) =>
            val newValue = i - yValue
            // if new value is zero, then remove the entry from the map
            if (newValue == 0) {
              map.-(c)
            } else {
              map.updated(c, i - yValue)
            }
        }
    }.toList.sorted
  }

  /** Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence
   *  `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *  List(
   *    List(en, as, my),
   *    List(en, my, as),
   *    List(man, yes),
   *    List(men, say),
   *    List(as, en, my),
   *    List(as, my, en),
   *    List(sane, my),
   *    List(Sean, my),
   *    List(my, en, as),
   *    List(my, as, en),
   *    List(my, sane),
   *    List(my, Sean),
   *    List(say, men),
   *    List(yes, man)
   *  )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as
   *  long as all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the
   *  anagram of itself, so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence. Note that the anagram of the empty sentence
   *  is the empty sentence itself.
   *
   * Hint: First of all, think about the recursive structure of the problem: what is the base case,
   * and how should the result of a recursive invocation be integrated in each iteration?
   * Also, using for-comprehensions helps in finding an elegant implementation for this method.
   *
   * Test the `sentenceAnagrams` method on short sentences, no more than 10 characters.
   * The combinations space gets huge very quickly as your sentence gets longer, so the program
   * may run for a very long time. However for sentences such as `Linux rulez`, `I love you`
   * or `Mickey Mouse` the program should end fairly quickly -- there are not many other ways to say
   * these things.
   *
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = sentence match {
    case Nil => List(Nil)
    case s =>
      // TODO
      val occurrences: Occurrences = sentenceOccurrences(s)
      println(occurrences)

      val tmp = for {
        // occurrence combinations
        occ <- combinations(occurrences)
        // Get the words that match this occurrence
        words <- dictionaryByOccurrences.get(occ)
      } yield words

      tmp foreach println

      List(Nil)
  }
}
