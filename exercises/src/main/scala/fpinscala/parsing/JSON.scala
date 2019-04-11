package fpinscala.parsing

import language.higherKinds
import language.implicitConversions
import scala.util.matching.Regex

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String) = token(P.string(s))

    def array = surround("[","]")(
      (value sep ",") map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj = surround("{","}")(
      (keyval sep ",") map {kvs =>println(kvs);JObject(kvs.toMap)}) scope "object"
    def keyval = escapedQuoted ** (":" *> value)
    def lit = scope("literal") {
      "null".as(JNull) |
      double.map(JNumber(_)) |
      escapedQuoted.map(JString(_)) |
      "true".as(JBool(true)) |
      "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}

object StringId {
  type Id[A] = A
  def stringParser[Parser[+_]](P: Parsers[Parser]): Parser[Id[String]] = {
    import P._
    // "abra" ** "cadabra" map{case (a, b) => 
    //   println(a)
    //   println(b)
    //   a + b
    // }
    // (string("abra") or string("cadabra"))
    // listOfN(2, string("ab") or string("cad")) map (a => a.mkString(""))
    // many(string("ab") or string("cad")) map (a => a.mkString(""))
    slice(("a"|"b").many)
  }
}

object MyParser {
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def extract: Either[ParseError,A] = this match {
      case Failure(e) => Left(e)
      case Success(a,_) => Right(a)
    }
    def uncommit: Result[A] = this match {
      case Failure(e) => Failure(e)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConusmed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}

import MyParser._

object MyParsers extends Parsers[Parser] {
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    if ( offset >= s1.length ) offset
    else {
      while (i < s1.length && i < s2.length) {
        if (s1.charAt(i+offset) != s2.charAt(i)) return i
        i += 1
      }
      if (s1.length-offset >= s2.length) -1
      else s1.length-offset
    }
  }

  def string(s: String): Parser[String] = {
    val msg = "'" + s + "'"
    l => {
      val i = firstNonmatchingIndex(l.input, s, l.offset)
      if (i == -1)
        Success(s, s.length)
      else
        Failure(l.advanceBy(i).toError(msg))

    }
  }
  def succeed[A](a: A): Parser[A] = l => Success(a, 0)
  def regex(r: Regex): Parser[String] = 
    l => {
      r.findPrefixOf(l.input) match {
        case Some(m) => Success(m, m.length)
        case None => Failure(l.toError(s"regex $r"))
      }
    }
  def slice[A](p: Parser[A]): Parser[String] = 
    l => {
      p(l) match {
        case Success(_, n) => 
          println(l)
          Success(l.input.substring(l.offset, l.offset + n), n)
        case f@Failure(_) => f
      }
    }
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = 
    l => {
      p1(l) match {
        case Failure(_) => p2(l)
        case r          => r
      }
    }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = 
    l => {
      p(l) match {
        case Success(i, n) => f(i)(l.advanceBy(n))
        case f@Failure(_) => f
      }

    }
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val s0 = Location(input)
    p(s0).extract

  }
  def label[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???
  def attempt[A](p: Parser[A]): Parser[A] = ???
}

object JSONParser extends Parsers[Parser] {
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    // if ( offset >= s1.length ) offset
    // else {
    //   while (i < s1.length && i < s2.length) {
    //     if (s1.charAt(i+offset) != s2.charAt(i)) return i
    //     i += 1
    //   }
    //   if (s1.length-offset >= s2.length) -1
    //   else s1.length-offset
    // }
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }

  def string(s: String): Parser[String] = {
    val msg = "'" + s + "'"
    l => {
      val i = firstNonmatchingIndex(l.input, s, l.offset)
      if (i == -1)
        Success(s, s.length)
      else
        Failure(l.advanceBy(i).toError(msg))

    }
  }
  def succeed[A](a: A): Parser[A] = l => Success(a, 0)
  def regex(r: Regex): Parser[String] = 
    l => {
      r.findPrefixOf(l.input) match {
        case Some(m) => Success(m, m.length)
        case None => Failure(l.toError(s"regex $r"))
      }
    }
  def slice[A](p: Parser[A]): Parser[String] = 
    l => {
      p(l) match {
        case Success(_, n) => 
          Success(l.input.substring(l.offset, l.offset + n), n)
        case f@Failure(_) => f
      }
    }
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A] = 
    l => {
      p1(l) match {
        case Failure(_) => p2(l)
        case r          => r
      }
    }

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = 
    l => {
      p(l) match {
        case Success(i, n) => f(i)(l.advanceBy(n))
        case f@Failure(_) => f
      }

    }
  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    val s0 = Location(input)
    p(s0).extract

  }
  def label[A](msg: String)(p: Parser[A]): Parser[A] =
    l => p(l) match {
      case Failure(a) => Failure(a.label(s"label: $a"))
      case s          => s
    }

  def scope[A](msg: String)(p: Parser[A]): Parser[A] =
   l => p(l) match {
     case Failure(a) => Failure(a.push(l, (s"scope: $a")))
     case s          => s
   }

  def attempt[A](p: Parser[A]): Parser[A] =
   l => p(l).uncommit 
   // s => p(s).uncommit
}
object JSONParserTest {
  val P = fpinscala.parsing.JSONParser
  import fpinscala.parsing.JSONParser._
   val json: Parser[JSON] = JSON.jsonParser(P)
   val jsonTxt = """
   {
     "Company name" : "Microsoft Corporation"
   }
   """
   def render() = {
     println(P.run(json)(jsonTxt))
   }
  
  // val stringParser: Parser[String] = StringId.stringParser(P)
  // def render() = {
  //   // println(P.run(stringParser)("abra"))
  //   // println(P.run(stringParser)("cadabra"))
  //   // println(P.run(stringParser)("ababcad"))
  //   println(P.run(stringParser)("aaba"))
  // }
}
