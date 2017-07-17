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

  "faltMap" should "flatten arrays" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) should be (List(1,1,2,2,3,3))
  }

  "hasSubsequece" should "be true for correct subsequence" in {
    List.hasSubsequence(List(1,2,3,4),List(1,2)) should be (true)
  }
  "hasSubsequece" should "be true for correct subsequence2" in {
    List.hasSubsequence(List(1,2,3,4),List(2,3)) should be (true)
  }
  "hasSubsequece" should "be true for correct for subsequence3" in {
    List.hasSubsequence(List(1,2,3,4),List(4)) should be (true)
  }
  "hasSubsequece" should "be false for incorrect subsequence" in {
    List.hasSubsequence(List(1,2,3,4),List(2,1)) should be (false)
  }
}
