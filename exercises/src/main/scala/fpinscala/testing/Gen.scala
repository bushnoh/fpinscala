package fpinscala.testing

//import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{ExecutorService, Executors}

import fpinscala.state.RNG.Rand

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Boolean
  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = p.check && Prop.this.check
  }
}

case class Gen[A](sample: State[RNG,A])

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] = Gen(State(RNG.map(RNG.nonNegativeLessThan(2))(_ == 0)))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    val rng: List[State[RNG, A]] = List.fill(n)(g.sample)
    val state: State[RNG, List[A]] = State.sequence(rng)
    Gen(state)
  }

  def choose(start: Int, stopExclusive: Int): Gen[Int] = { //Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
    val rng: Rand[Int] = RNG.map(RNG.nonNegativeInt)(n => start + n % (stopExclusive-start))
    val randstate = (r: RNG) => rng(r)
    val state = State(randstate)
    Gen(state)
  }
}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

trait SGen[+A] {

}

