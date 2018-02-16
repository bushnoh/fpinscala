package fpinscala.parsing

import fpinscala.testing.Gen

import language.higherKinds
import fpinscala.testing.Prop
import fpinscala.testing.Prop._

import scala.util.matching.Regex


trait Parsers[Parser[+ _]] {
  self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
  ParserOps[String] = ParserOps(f(a))

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a.flatMap(_a => succeed(f(_a)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p.flatMap(a => p2.map(b => f(a,b)))

  implicit def regex(r: Regex): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] = {
    val pchain = map2(p, many(p))(_ :: _)
    val por = or(pchain,succeed(List()))
    por
  }

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = n match {
    case nn if n <= 0 => succeed(List())
    case nn => map2(p, listOfN(nn-1, p))(_ :: _)
  }

  def slice[A](p: Parser[A]): Parser[String]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def separated[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] = separated1(p, s) | succeed(List())


  def lhs[A, B](a: Parser[A], b: Parser[B]): Parser[A] = map2(a, b)((_a, _) => _a)

  def rhs[A, B](a: Parser[A], b: Parser[B]): Parser[B] = map2(a, b)((_, _b) => _b)

  def wrap[A](l: Parser[Any], r: Parser[Any])(a: Parser[A]): Parser[A] = lhs(rhs(l, a), r)
  val eof: Parser[String] = regex("\\z".r)
  val whitespace: Parser[String] = regex("\\s*".r)
  val double: Parser[String] = regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)
  val quotedstring: Parser[String] = wrap("\"", "\"")(regex("[^\"]+".r))

  def wswrap[A](a: Parser[A]): Parser[A] = wrap(whitespace, whitespace)(a)

  def separated1[A](p: Parser[A], s: Parser[Any]): Parser[List[A]] = {
    map2(p, many(rhs(s, p)))(_ :: _)
  }

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2) // use `self` to explicitly disambiguate reference to the `or` method on the `trait`
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def label(msg: String): Parser[A] = self.label(msg)(p)

    def scope(msg: String): Parser[A] = self.scope(msg)(p)

    def many = self.many(p)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      self.product(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

  object AdHoc {
    def contextSensitiveParser: Parser[String] = {
      regex("[0-9]".r).map(_.toInt).flatMap(n => listOfN(n, char('a'))).map(_.mkString)
    }
  }

}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.lines.drop(line - 1).next
    else ""
}

case class ParseError(stack: List[(Location, String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}