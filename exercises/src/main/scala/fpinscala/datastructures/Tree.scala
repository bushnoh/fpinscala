package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]):Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  def maxInt(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l,r) => maxInt(l) max maxInt(r)
  }

  def maxDepth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (maxDepth(l) max maxDepth(r))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }

  def fold[A,B](t: Tree[A])(f: A=> B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
  }

  def size2[A](t: Tree[A]):Int = fold(t)(v => 1)(1+_+_)

  def maxInt2(t: Tree[Int]): Int = fold(t)(v => v)(_ max _)

  def maxDepth2[A](t: Tree[A]): Int = fold(t)(v => 0)((l,r) => 1 + l max r)

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)):Tree[B])(Branch(_,_))

}