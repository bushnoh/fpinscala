package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (i, _rng) if i >= 0 => (i, _rng)
    case (i, _rng) => (-(i + 1), _rng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val i = nonNegativeInt(rng)
    (i._1.toDouble / (Int.MaxValue + 1), i._2)
  }

  def doubleMap: Rand[Double] = map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val nI = rng.nextInt
    val nD = double(nI._2)
    ((nI._1, nD._1), nD._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val nD = double(rng)
    val nI = nD._2.nextInt
    ((nD._1, nI._1), nI._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(acc: List[Int], currRng: RNG, c: Int): (List[Int], RNG) = c match {
      case n if n >= 0 => (acc, currRng)
      case n => {
        val (nI, nRng) = currRng.nextInt
        loop(nI :: acc, nRng, n - 1)
      }
    }

    loop(List(), rng, count)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (av, ar) = ra(rng)
    val (bv, br) = rb(ar)
    (f(av, bv), br)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    def loop(acc: List[A], trans: List[Rand[A]], currRng: RNG): (List[A], RNG) = trans match {
      case Nil => (acc, currRng)
      case h :: t => {
        val (v, r) = h(currRng)
        loop(v :: acc, t, r)
      }
    }

    loop(List(), fs, rng)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, z) => map2(f, z)(_ :: _))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (v, r) = f(rng)
    g(v)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => mapViaFlatMap(rb)(b => f(a,b)))

}

import State._

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a,s2) = run(s)
    f(a).run(s2)
  })
  def get[S]: State[S, S] = State(s => (s, s))


}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def update(i: Input): Machine = (i,this) match {
    case (_,Machine(_,c,_)) if c <= 0 => this
    case (Turn,Machine(true,_,_)) => this
    case (Coin,Machine(false,_,_)) => this
    case (Coin,Machine(true,c,cn)) => Machine(false,c,cn+1)
    case (Turn,Machine(false,c,cn)) => Machine(true,c-1,cn)
  }

}

object State {
  type Rand[A] = State[RNG, A]

  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f,z) => f.map2(z)(_ :: _))

  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

}

object Machine {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val li: State[Machine, List[Unit]] = sequence(inputs.map((i:Input) => State((m: Machine) => ((),m.update(i)))))
    val ns: State[Machine, Machine] = li.get
    ns.map(m => (m.candies,m.coins))
  }

}