package example

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {
  import Lists._
  
  test("sum of a few numbers") {
    assert(sum(List(1,2,0)) === 3)
  }
  test("max of a few numbers") {
    assert(max(List(3, 7, 2)) === 7)
  }
}
