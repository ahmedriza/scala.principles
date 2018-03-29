package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1: Set = singletonSet(1)  // (1)
    val s2: Set = singletonSet(2)  // (2)
    val s3: Set = singletonSet(3)  // (3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet membership") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton s1")
      assert(!contains(s1, 2), "Singleton s1 does not contain 2")

      assert(contains(s2, 2), "Singleton s2")
      assert(!contains(s2, 1), "Singleton s2 does not contain 1")
      assert(!contains(s2, 3), "Singleton s2 does not contain 3")

      assert(contains(s3, 3), "Singleton s3")
      assert(!contains(s3, 1), "Singleton s3 does not contain 1")
      assert(!contains(s3, 2), "Singleton s3 does not contain 2")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s: Set = union(s1, s2)         // (1, 2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains elements common to both sets") {
    new TestSets {
      val nullSet: Set = intersect(s1, s2)


      val s4: Set = union(s1, s2) // (1, 2)
      val s5: Set = intersect(s1, s4)
      assert(contains(s5, 1), "s4 intersect s1 contains 1")
      assert(!contains(s5, 2), "s4 intersect s1 does not contain 2")
    }
  }

  test("diff contains elements in 1st set but not in 2nd set") {
    new TestSets {
      val s4: Set = union(union(s1, s2), s3) // (1, 2, 3)

      val s5: Set = diff(s4, s2) // (1, 3)
      assert(contains(s5, 1), "s5 contains 1")
      assert(contains(s5, 3), "s5 contains 3")
      assert(!contains(s5, 2), "s5 does not contain 2")
    }
  }

  test("filter contains elements in S that satisfy predicate P") {
    new TestSets {
      val s4: Set = union(union(s1, s2), s3) // (1, 2, 3)

      val s5: Set = filter(s4, x => x % 2 == 0)
      assert(contains(s5, 2), "s5 contains 2")
      assert(!contains(s5, 1), "s5 does not contain 1")
      assert(!contains(s5, 3), "s5 does not contain 3")

      val s6: Set = filter(s4, x => x % 2 != 0)
      assert(contains(s6, 1), "s6 contains 1")
      assert(contains(s6, 3), "s6 contains 3")
      assert(!contains(s6, 2), "s6 does not contain 2")
    }
  }

  test("forall") {
    val s1: Set = union(union(singletonSet(2), singletonSet(4)), singletonSet(6)) // (2, 4, 6)
    assert(forall(s1, x => x % 2 == 0), "elements of s1 are all even")

    val s2: Set = union(union(singletonSet(2), singletonSet(4)), singletonSet(7)) // (2, 4, 7)
    assert(!forall(s2, x => x % 2 == 0), "elements of s2 are not all even")
  }

  test("exists") {
    val s1: Set = union(union(singletonSet(1), singletonSet(5)), singletonSet(7)) // (1, 5, 7)
    assert(!exists(s1, x => x % 2 == 0), "No element in s1 is even")

    val s2: Set = union(union(singletonSet(1), singletonSet(4)), singletonSet(7)) // (1, 4, 7)
    assert(exists(s2, x => x % 2 == 0), "at least one element in s2 is even")
  }

  test("map") {
    val s1: Set = union(union(singletonSet(1), singletonSet(5)), singletonSet(7)) // (1, 5, 7)
    val s2: Set = map(s1, x => x + 2)

    printSet(s1)
    // printSet(s2)

    val s3: Int => Boolean = x => x == 1 || x == 5 || x == 7
    printSet(s3)

  }
}
