package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by abush on 29/04/17.
  */
class TestDataStructures extends FlatSpec with Matchers {

  "reverse" should "return reversed list" in {
    List.reverse(List(1, 9, 4)) should be (List(4,9,1))
  }

  "reverse2" should "return reversed list" in {
    List.reverse2(List(1, 9, 4)) should be (List(4,9,1))
  }
}
