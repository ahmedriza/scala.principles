package week3

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListSuite extends FunSuite {

  test("MyNil has no head") {
    val nil = new MyNil[Int]()
    assertThrows[NoSuchElementException] {
      nil.head
    }
  }

  test("MyNil has no tail") {
    val nil = new MyNil[Int]()
    assertThrows[NoSuchElementException] {
      nil.tail
    }
  }

}

