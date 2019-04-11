package fpinscala.parsing

import java.util.regex._
import scala.util.matching.Regex

import language.higherKinds
import fpinscala.testing.Prop._
import fpinscala.testing.Gen._
import fpinscala.testing._

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  // primitives
  implicit def string(s: String): Parser[String]
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def succeed[A](a: A): Parser[A]
  def or[A](a: Parser[A], b: => Parser[A]): Parser[A]

  def run[A](a: Parser[A])(input: String): Either[ParseError, A]
  // end primitives

  //                                                  def lazyWrap[B](a: => Parser[B]): Parser[B]
  //                                                  could it be with lazyWrap
                                                //    ___^__
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(List())// lazyWrap has the same effect as the second argument than map2
  def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = 
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)
  // implicit def char(c: Char): Parser[Char]
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p1.flatMap(a => p2.map(b => (a,b)))
  // def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C) = product(p1, p2).map(f.tupled) //.map{case (a,b) => f(a,b)}
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C) = p1.flatMap(a => p2.map(b => f(a,b)))
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  // val _numA: Parser[Int] = char('a').many.map(_.size)
  // val numA: Parser[Int] = char('a').many.slice.map(_.size)

  def eof: Parser[String] = regex("\\z".r).label("unexpected trailing characters")

  def root[A](p: Parser[A]): Parser[A] = p <* eof

  def doubleString: Parser[String] = token("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r)

  def double: Parser[Double] = doubleString map (_.toDouble) label "double literal"

  def thru(s: String): Parser[String] = (".*?"+Pattern.quote(s)).r

  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  def escapedQuoted: Parser[String] = token(quoted label "string literal")

  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = 
    sep1(p,p2) or succeed(List())

  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def whitespace: Parser[String] = "\\s*".r

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace
  
  //Deal with errors
  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]


  implicit class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def many:Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def sep(separator: Parser[Any]) = self.sep(p, separator)
    def sep1(separator: Parser[Any]) = self.sep1(p, separator)
    def scope(msg: String): Parser[A] = self.scope(msg)(p)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
  }

  object Laws {
    private def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
    private def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)
    // def errorMessage(e: ParseError): String

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = 
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def unitLaw[A](a: A)(in: Gen[String]): Prop = 
      forAll(in)(s => run(succeed(a))(s) == Right(a))

    def associativityLaw[A, B, C](p1: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]): Prop = 
      equal((p1 ** p2) ** p3 map (unbiasL), p1 ** (p2 ** p3) map (unbiasR))(in)

    def mapProductLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      equal(p1.map(identity) ** p2.map(identity), (p1 ** p2) map {case (a, b) => (a, b)} )(in)

    // def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
    //   forAll(inputs ** Gen.string){case (input, msg) => 
    //     run(label(msg)(p))(input) match {
    //       case Left(e) => errorMessage(e) == msg
    //       case _       => true
    //     }  
    //   }

  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)
}
