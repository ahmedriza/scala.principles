
import Reduction._

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class ReductionTestSuite extends FunSuite {

  test("sum") {
    val s1 = sum(List(1,2,3))
    assert(s1 === 6)
  }

  test("product") {
    val s1 = product(List(1,2,3,4))
    assert(s1 === 24)
  }

  test("foldLeft") {
    val list = List(1,2,3,4)
    assert(foldLeft(list, 0)(_ + _) === 10)
    assert(list.foldLeft(0)(_ + _) === 10)

    assert(list.reduceLeft(_ + _) === 10)
  }

  test("foldRight") {
    val list = List(1,2,3,4)
    assert(foldRight(list, 0)(_ + _) === 10)
  }
}
