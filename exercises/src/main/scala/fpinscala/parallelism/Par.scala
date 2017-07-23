package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A, B, C](a: Future[A], b: Future[B])(f: (A, B) => C) extends Future[C] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = res match {
      case Some(c) => c
      case None => {
        val duration = TimeUnit.MILLISECONDS.convert(timeout, units)
        val start = System.currentTimeMillis()
        val ga = a.get(duration, TimeUnit.MILLISECONDS)
        val delta = System.currentTimeMillis() - start
        val gb = b.get(duration - delta, TimeUnit.MILLISECONDS)
        val c = f(ga, gb)
        res = Some(c)
        c
      }
    }

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean = {
      val ac = a.cancel(evenIfRunning)
      b.cancel(evenIfRunning) && ac
    }

    override def get(): C = get(Long.MaxValue, TimeUnit.MILLISECONDS)

    var res: Option[C] = None

  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }

  def map2Timeout[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf)(f)
    }

  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2Timeout(h, fork(sequence(t)))(_ :: _)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val l: Par[List[List[A]]] = parMap(as)(e => if (f(e)) List(e) else Nil)
    map(l)(_.flatten)
  }

  def fold[A](z: A)(list: IndexedSeq[Par[A]])(f: (A, A) => A): Par[A] = list match {
    case l if l.length == 0 => unit(z)
    case l if l.length == 1 => map2(l.head, unit(z))(f)
    case l => {
      val (ls, rs) = l.splitAt(l.length / 2)
      val fls = fork(fold(z)(ls)(f))
      val frs = fork(fold(z)(rs)(f))
      map2(fls, frs)(f)
    }
  }

  def parSum(ints: IndexedSeq[Int]): Par[Int] = {
    es: ExecutorService => fold(0)(ints.map(unit(_)))(_ + _)(es)
  }

  def parMax(ints: IndexedSeq[Int]): Par[Int] = {
    es: ExecutorService => fold(Int.MinValue)(ints.map(unit(_)))((a, b) => a.max(b))(es)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val choice = run(es)(n).get
      choices(choice)(es)
    }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val n: Par[Int] = es => if (run(es)(cond).get) unit(0)(es) else unit(1)(es)
    val n2: Par[Int] = map(cond)(b => if (b) 0 else 1)
    choiceN(n2)(List(t,f))
  }

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es => {
      val k = run(es)(key).get
      choices(k)(es)
    }
  }

  def chooser[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val a = run(es)(pa).get
      choices(a)(es)
    }
  }

  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val a = run(es)(pa).get
      choices(a)(es)
    }
  }

  def flatMapViaJoin[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    join(map(pa)(choices))
  }

  def joinViaFlatmap[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(a => a)
  }

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val outer = run(es)(a).get
      run(es)(outer)
    }

  def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val n2: Par[Int] = map(cond)(b => if (b) 0 else 1)
    chooser(n2)(List(t,f))
  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {


  }

}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sum2(ints: IndexedSeq[Int]): Int = parSum(ints)(Executors.newFixedThreadPool(6)).get

  def max(ints: IndexedSeq[Int]): Int = parMax(ints)(Executors.newFixedThreadPool(6)).get


}
