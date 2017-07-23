//package fpinscala.laziness
//
//import Stream._
//
//trait Stream[+A] {
//
//  def toList: List[A] = this match {
//    case Cons(h, t) => h() :: t().toList
//    case _ => List()
//  }
//
//  def toListSafe: List[A] = {
//    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
//      case Cons(h, t) => loop(t(), h() :: acc)
//      case _ => acc
//    }
//
//    loop(this, List()).reverse
//  }
//
//  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
//    this match {
//      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
//      case _ => z
//    }
//
//  def exists(p: A => Boolean): Boolean =
//    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
//
//  @annotation.tailrec
//  final def find(f: A => Boolean): Option[A] = this match {
//    case Empty => None
//    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
//  }
//
//  @annotation.tailrec
//  final def drop(n: Int): Stream[A] = (n, this) match {
//    case (_, Empty) => this
//    case (_, _) if n < 0 => this
//    case (_, Cons(h, t)) => t().drop(n - 1)
//  }
//
//  def take(n: Int): Stream[A] = this match {
//    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
//    case Cons(h, t) if n == 0 => cons(h(), empty)
//    case _ => empty
//  }
//
//  def dropWhile(p: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) if p(h()) => t().dropWhile(p)
//    case _ => this
//  }
//
//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
//    case _ => empty
//  }
//
//  def forAll(p: A => Boolean): Boolean = this match {
//    case empty => true
//    case Cons(h, t) if p(h()) => t().forAll(p)
//    case _ => false
//  }
//
//  def forAllFold(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)
//
//  def takeWhileFold(p: A => Boolean): Stream[A] = foldRight(empty: Stream[A])((a, b) => if (!p(a)) b else cons(a, b))
//
//  def takeWhileUnfold(p: A => Boolean): Stream[A] = unfold(this) {
//    case Cons(h, t) if p(h()) => Some((h(), t()))
//    case _ => None
//  }
//
//  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, s2) {
//    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
//    case _ => None
//  }
//
//  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold(this,s2){
//    case (Cons(h1,t1),Cons(h2,t2)) => Some((Some(h1()),Some(h2())),(t1(),t2()))
//    case (Cons(h1,t1),Empty) => Some((Some(h1()),None[B]),(t1(),Empty[B]))
//    case (Empty,Cons(h2,t2)) => Some((None[A],Some(h2())),(Empty[A],t2()))
//    case (Empty,Empty) => None
//  }
//
//
//  def headOption: Option[A] = foldRight(None[A])((a, b) => Some(a))
//
//  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
//  // writing your own function signatures.
//
//  //def map[A, B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
//
//  def mapUnfold[B](f: A => B): Stream[B] = unfold(this)((s: Stream[A]) => s match {
//    case empty => None
//    case Cons(h, t) => Some((f(h()), t()))
//  })
//
//  def filter[A](f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)
//
//  def append[A](b: => Stream[A]): Stream[A] = foldRight(b)((h, t) => cons(h, t))
//
//  def flatMap[A, B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h).append(t))
//
//  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forAll{
//    case (h1,h2) => h1 == h2
//  }
//
//  def tails: Stream[Stream[A]] = unfold(this){
//    case Cons(h,t) => Some(cons(h(),t()),t())
//    case _ => None
//  }
//
//  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(z -> Stream(z))((a,p0) => {
//    val b2 = f(a, p0._1)
//    (b2,cons(b2,p0._2))
//  })._2
//}
//
//case object Empty extends Stream[Nothing]
//
//case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
//
//object Stream {
//  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
//    lazy val head = hd
//    lazy val tail = tl
//    Cons(() => head, () => tail)
//  }
//
//  def empty[A]: Stream[A] = Empty
//
//  def apply[A](as: A*): Stream[A] =
//    if (as.isEmpty) empty
//    else cons(as.head, apply(as.tail: _*))
//
//  val ones: Stream[Int] = Stream.cons(1, ones)
//
//  def constant[A](a: A): Stream[A] = {
//    lazy val const = Stream.cons(a, const)
//    const
//  }
//
//  def from(n: Int): Stream[Int] = {
//    lazy val tail = Cons(() => n, () => from(n + 1))
//    tail
//  }
//
//  def fibs: Stream[Int] = {
//    def loop(p2: Int, p1: Int): Stream[Int] = {
//      lazy val p0 = p1 + p2
//      cons(p0, loop(p1, p0))
//    }
//
//    cons(0, cons(1, loop(0, 1)))
//  }
//
//  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
//    case None => empty
//    case Some(h) => cons(h._1, unfold(h._2)(f))
//  }
//
//  def fibsByUnfold: Stream[Int] = unfold((0, 1))((s: (Int, Int)) => Some((s._1), (s._2, s._1 + s._2)))
//
//  def fromByfold(n: Int): Stream[Int] = unfold(n) { case n => Some(n, n + 1) }
//
//  def consByFold(c: Int): Stream[Int] = unfold(c) { case c => Some(c, c) }
//
//  lazy val onesViaFold = consByFold(1)
//
//
//}