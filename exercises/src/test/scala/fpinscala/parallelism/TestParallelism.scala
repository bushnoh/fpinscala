package fpinscala.parallelism

import fpinscala.parallelism.Examples
import org.scalatest._

/**
  * Created by abush on 29/04/17.
  */
class TestParallelism extends FlatSpec with Matchers {

  "sum2" should "give correct result" in {
    Examples.sum2(IndexedSeq(1,2,3,4)) should be(10)
  }


  "max" should "give correct result" in {
    Examples.max(IndexedSeq(1,2,3,4,4,1,7,3,2,3)) should be(7)
  }

}
