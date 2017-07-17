package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => Nil
        case Cons(_, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => l
    case Cons(a, Cons(b, Nil)) => Cons(a, Nil)
    case Cons(a, b) => Cons(a, init(b))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((l, z) => z + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((z, l) => z + 1)

  def reverse[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], r: List[A]): List[A] = l match {
      case Nil => r
      case Cons(h,t) => loop(t,Cons(h,r))
    }
    loop(l,Nil)
  }

  def reverse2[A](l: List[A]): List[A] = foldLeft(l,List[A]())((r,l) => Cons(l,r))

  def addOneToList(l: List[Int]): List[Int] = foldRight(l,Nil:List[Int])((h,t) => Cons(h+1,t))

  def toString(l: List[Double]): List[String] = foldRight(l,Nil:List[String])((h,t) => Cons(h.toString,t))

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l,Nil:List[B])((h,t) => Cons(f(h),t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l,Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = reverse(foldLeft(as,Nil:List[B])((t,h) => foldRight(f(h),t)((h1,t1) => Cons(h1,t1))))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)( e => if (f(e)) Cons(e,Nil) else Nil)

  def addLists(la: List[Int], lb: List[Int]): List[Int] = (la,lb) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(ha,ta),Cons(hb,tb)) => Cons(ha+hb,addLists(ta,tb))
  }

  def zipWith[A,B,C](la: List[A],lb: List[B])(f: (A,B) => C): List[C] = (la,lb) match {
    case (Nil,_) => Nil
    case (_,Nil) => Nil
    case (Cons(ha,ta),Cons(hb,tb)) => Cons(f(ha,hb),zipWith(ta,tb)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
    case (_,Nil) => true
    case (Nil,_) => false
    case (Cons(hsup,tsup),Cons(hsub,tsub)) => {
      if (hsup == hsub) {
        if (hasSubsequence(tsup,tsub)) true
        else hasSubsequence(tsup,sub)
      }
      else hasSubsequence(tsup,sub)
    }
  }
}
