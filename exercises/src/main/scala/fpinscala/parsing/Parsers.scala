package fpinscala.parsing

import fpinscala.parsing.Parsers.Parser
import fpinscala.testing._

import language.{higherKinds, implicitConversions}
import scala.util.matching.Regex

sealed trait Result[+A] {
  def mapError(f: ParseError => ParseError): Result[A] = this match {
    case Failure(e) => Failure(f(e))
    case _ => this
  }
}
case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
case class Failure(get: ParseError) extends Result[Nothing]
object Parsers {
  type Parser[+A] = Location => Result[A]
}

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait

  implicit def string(s: String): Parser[String]
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def succeed[A](a: A): Parser[A] = string("") map (_ => a)
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def attempt[A](p: Parser[A]): Parser[A]
  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  //Exercise 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n-1, p))(_ :: _)

  //Exercise 9.3
  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p)(a => map(p2)(b => (a,b)))

  def slice[A](p: Parser[A]): Parser[String]

  val numA: Parser[Int] = char('a').many.map(_.size)
  val aNums: Parser[Int] = for {
    digit <- "[0-9]".r
    val n = digit.toInt
    _ <- listOfN(n, char('a'))
  } yield n

  //Exercise 9.1
  // product is primitive, implementing map2 in term on product
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
//    (p ** p2).map(f.tupled)
  flatMap(p)(a => map(p2)(b => f(a,b)))


  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p).map(_.drop(1))) { _ :: _}

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ParseError): String
  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def many: Parser[List[A]] = self.many(p) }

  object Laws {
    import Prop.forAll
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop = forAll(in)(s => run(succeed(a))(s) == Right(a))

    def labelLaw[A](p: Parser[A], inputs: SGen[String]): Prop =
      forAll(inputs ** Gen.string) { case (input, msg) =>
          run(label(msg)(p))(input) match {
            case Left(e) => errorMessage(e) == msg
            case _ => true
          }

      }
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

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_,s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location,String)] =
    stack.lastOption
}

object MyParser extends Parsers[Parser] {
  override implicit def string(s: String): Parser[String] = ???

  override implicit def regex(r: Regex): Parser[String] = ???

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???

  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.la)

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override def slice[A](p: Parser[A]): Parser[String] = ???

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ParseError): String = ???
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}