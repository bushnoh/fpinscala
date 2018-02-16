package fpinscala.testing

import fpinscala.laziness.Stream
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


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (max, cases, rand) =>
      run(max, cases, rand) match {
        case Passed | Proved => p.run(max, cases, rand)
        case x => x
      }
  }

  def ||(p: Prop): Prop = Prop {
    (max, cases, rand) =>
      this.run(max, cases, rand) match {
        case Falsified(e, _) => p.tag(e).run(max, cases, rand)
        case x => x
      }
  }

  def tag(msg: String) = Prop {
    (max, cases, rng) =>
      run(max, cases, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
  }
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def listOfN(size: Int): Gen[List[A]] = {
    Gen.listOfN(size, this)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => this.listOfN(n))
  }

  def map2[B,C](g: Gen[B])(f: (A,B) => C): Gen[C] =
    Gen(sample.map2(g.sample)(f))

  def unsized: SGen[A] = SGen(_ => this)
}

object Prop {
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  /* Produce an infinite random stream from a `Gen` and a starting `RNG`. */
  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }


  // String interpolation syntax. A string starting with `s"` can refer to
  // a Scala value `v` as `$v` or `${v}` in the string.
  // This will be expanded to `v.toString` by the Scala compiler.
  def buildMsg[A](s: A, e: Exception): String =
  s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n - 1) / max + 1
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, n, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }



  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool
  val p1 = Prop.forAll(Gen.unit(Par.unit(1)))(i =>
    Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }
  val S = weighted(
    choose(1,4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

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
    val rng: Rand[Int] = RNG.map(RNG.nonNegativeInt)(n => start + n % (stopExclusive - start))
    val randstate = (r: RNG) => rng(r)
    val state = State(randstate)
    Gen(state)
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    boolean.flatMap(b => if (b) g1 else g2)
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1frac = (g1._2.abs) / (g1._2.abs + g2._2.abs)
    val fracMap = Gen(State(RNG.double))
    fracMap.flatMap(frac => {
      if (frac <= g1frac) g1._1 else g2._1
    })
  }



}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = {
    SGen((i: Int) => forSize(i).map(f))
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    SGen((i: Int) => forSize(i).flatMap(a => f(a).forSize(i)))
  }
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen((i: Int) => listOfN(i, g))
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen((i: Int) => g.listOfN(i max 1))
  }
}

object Run {
  def main(args: Array[String]): Unit = {
    val smallInt = Gen.choose(-10,10)
    val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)

    val listProp = forAll(SGen.listOf1(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.foldLeft((true,Int.MinValue)){(z,v) => z._1 match {
        case false => (false,v)
        case true => (z._2 <= v,v)
      }}._1
    }
    Prop.run(listProp)

    val firstLaw = check{
      val es: ExecutorService = Executors.newCachedThreadPool
      Par.equal(es)(Par.map(Par.unit(1))(_ + 1),Par.unit(2))
    }
    Prop.run(firstLaw)

    val pint = Gen.choose(0,10) map (Par.unit(_))
    val p4 =
      forAllPar(pint)(n => equal(Par.map(n)(y => y), n))
    Prop.run(p4)

    val lints = Gen.listOfN(1,smallInt)
    val pimp = forAllPar(lints)(l => {
      val inx = l.toIndexedSeq
      val parsum = Par.parSum(inx)
      val linsum = Par.unit(inx.sum)
      Par.map2(parsum,linsum)((p,l) => p == l)
    })
    Prop.run(pimp)

    val pfork = forAllPar(smallInt)(i => {
      Par.map2(Par.fork(Par.unit(i)),Par.unit(i))((f,u) => f == u)
    })
    Prop.run(pfork)
    println("ddd")
    System.exit(0)
  }

}
