package idealized.scala

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers._

@RunWith(classOf[JUnitRunner])
class NatTest extends FunSuite {

  trait Fixture {
    val one: Nat = Zero.successor
    val two: Nat = one.successor
    val three: Nat = two.successor
    val four: Nat = three.successor
    val five: Nat = four.successor
  }

  test("0 successor is 1") {
    val zero = Zero
    assert(zero.successor === new Succ(Zero))
  }

  test("4 - 2 = 1") {
    new Fixture {
      assert(two + two === four)
      assert(four - two === two)
    }
  }

  test("5 - 5 = 0") {
    new Fixture {
      assert(five - five == Zero)
    }
  }

  test("4 - 5 should throw") {
    new Fixture {
      assertThrows[IllegalArgumentException] {
        val result = four - five
      }
    }
  }


}
