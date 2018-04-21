
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class ReductionTestSuite extends FunSuite {

  test("sum") {
    val s1 = Reduction.sum(List(1,2,3))
    assert(s1 === 6)
  }
}
