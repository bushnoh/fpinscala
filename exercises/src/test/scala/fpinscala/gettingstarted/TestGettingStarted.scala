package fpinscala.gettingstarted

import org.scalatest._

/**
  * Created by abush on 29/04/17.
  */
class TestGettingStarted extends FlatSpec with Matchers {
  def gt(a: Int, b: Int ): Boolean = a > b

  "fib" should "give correct result" in {
    MyModule.fib(3) should be(2)
  }

  "isSorted" should "return true for sorted int array" in {
    PolymorphicFunctions.isSorted(Array(1, 4, 9), gt) should be(
      true)
  }

  "isSorted" should "return true for sorted int array with same elems" in {
    PolymorphicFunctions.isSorted(Array(1, 4, 4, 9), gt) should be(
      true)
  }

  "isSorted" should "return false for unsorted int array" in {
    PolymorphicFunctions.isSorted(Array(1, 9, 4), gt) should be(
      false)
  }

  "curry" should "return a function that currys one of the args" in {
    val oneGt = PolymorphicFunctions.curry(gt)(1)
    oneGt(2) should be (false)
  }

  "uncurry" should "return a function that uncuries the function, narturally" in {
    val cGt = PolymorphicFunctions.curry(gt)
    PolymorphicFunctions.uncurry(cGt)(1,2) should be (false)
  }


}
