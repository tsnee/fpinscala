package fpinscala.parsing

import language.higherKinds
import language.implicitConversions

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
  implicit def string(s: String): Parser[String]
  implicit def regex(r: scala.util.matching.Regex): Parser[String]
  def slice[A](p: Parser[A]): Parser[String]
  def label[A](m: String)(p: Parser[A]): Parser[A]
  def scope[A](m: String)(p: Parser[A]): Parser[A]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def attempt[A](p: Parser[A]): Parser[A]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap(x => succeed(f(x)))
  def setIssueLevel[A](a: Parser[A], e: IssueLevel): Parser[A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.head)
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n == 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)
  //def aStar[A](a: Parser[A]): Parser[Int]
  //def charPlus(c: Parser[Char]): Parser[Int] = map(seq(c, aStar(c)))(_.sum)
  //def seq[A](parsers: Parser[A]*): Parser[Seq[Int]]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] = for {
    a <- p
    b <- p2
  } yield (a, b)
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = product(p, p2).map(f.tupled)
  def map2ViaFlatMap[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = for {
    a <- p
    b <- p2
  } yield (f(a, b))
  def many[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _) or succeed(Nil)
  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)
  def succeed[A](a: A): Parser[A] = string("").map(_ => a)
  def thatManyAs: Parser[List[Char]] = regex(raw"\d+".r).flatMap(s => listOfN(s.toInt, char('a')))
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  object Laws {
    def mapIdentity[A](x: Parser[A], input: String): Boolean = run(map(x)(identity))(input) == run(x)(input)
    def caseSensitive(p: Parser[Char], c: Char): Boolean = run(map(p)(_.toUpper))(c.toLower.toString).isLeft
    def orCommutativity[A](x: Parser[A], y: Parser[A], input: String): Boolean =
      run(x | y)(input) == run(y | x)(input)
    def orAssociativity[A](x: Parser[A], y: Parser[A], z: Parser[A], input: String): Boolean =
      run((x | y) | z)(input) == run(x | (y | z))(input)
//def aStarEmpty[A](p: Parser[A]): Boolean = run(aStar(p))("") == Right(0)
//def charPlusNonEmpty(p: Parser[Char]): Boolean = run(charPlus(p))("").isLeft
//def seqNumbersCorrectly[A](ps: Seq[Parser[A]], input: String): Boolean =
//  run(seq(ps: _*))(input).map(_.length) == Right(ps.length)
    def productLaw[A,B](pa: Parser[A], pb: Parser[B], input: String): Boolean =
      run(product(pa, pb).slice)(input) == Right(input)
    def productAssociativity[A,B,C](pa: Parser[A], pb: Parser[B], pc: Parser[C], input: String): Boolean =
      run(product(pa, product(pb, pc)).slice)(input) == run(product(product(pa, pb), pc).slice)(input)
    def map2ProductEquivalence[A,B](pa: Parser[A], pb: Parser[B], input: String): Boolean =
      run(product(pa, pb))(input) == run(map2(pa, pb)(( _, _)))(input)
  }

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def map[B](f: A => B) = self.map(p)(f)
    def many: Parser[List[A]] = self.many(p)
    def slice: Parser[String] = self.slice(p)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def setIssueLevel(l: IssueLevel): Parser[A] = self.setIssueLevel(p, l)
  }
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val jNull: Parser[JSON] = string("null").map(_ => JNull)
    val jNumber: Parser[JSON] = regex(raw"[+-]?\d+(\.\d+)?".r).map(s => JNumber(s.toDouble))
    val jString: Parser[JString] = for {
      _ <- char('"')
      s <- regex(raw"""[^"]*""".r)
      _ <- char('"')
    } yield JString(s)
    val jBool: Parser[JSON] = for {
      b <- string("true") | string("false")
    } yield JBool(b.toBoolean)
    def jArray: Parser[JSON] = for {
      _ <- spaces
      _ <- char('[')
      _ <- spaces
      j <- product(json, product(char(','), json).map(_._2).many).map {
        case (hd, tl) => (hd :: tl).toIndexedSeq
      }
      _ <- spaces
      _ <- char(']')
    } yield JArray(j)
    def keyValuePair = for {
      _ <- spaces
      k <- jString
      _ <- spaces
      _ <- char(':')
      _ <- spaces
      v <- json
    } yield k.get -> v
    def jObject: Parser[JSON] = for {
      _ <- spaces
      _ <- char('{')
      _ <- spaces
      keyValuePairs <- product(keyValuePair, product(char(','), keyValuePair).map(_._2).many).map { case (hd, tl) => (hd :: tl) }
      _ <- spaces
      _ <- char('}')
    } yield JObject(keyValuePairs.toMap)
    def json: Parser[JSON] = jNull | jNumber | jString | jBool | jArray | jObject
    jObject
  }
}

sealed trait IssueLevel
object IssueLevel {
  case object Off extends IssueLevel
  case object Smell extends IssueLevel
  case object Warning extends IssueLevel
  case object Fatal extends IssueLevel
}

case class Location10(line: Int, column: Int)

case class Issue(description: String, level: IssueLevel, loc: Location10)

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
                      otherFailures: List[ParseError] = List())

trait MyParser[+A] {
  def issueLevel: IssueLevel = IssueLevel.Warning
  def scopes: List[String] = Nil
  def setIssueLevel(issueLevel: IssueLevel): MyParser[A] =
    ParserDecorator(underlying = this, issueLevel = issueLevel)
  def setLabel(label: String): MyParser[A] =
    ParserDecorator(underlying = this, label = label)
  def pushScope(scope: String): MyParser[A] =
    ParserDecorator(underlying = this, scopes = scope :: scopes)
  def popScope(scope: String): MyParser[A] =
    ParserDecorator(underlying = this, scopes = scopes.tail)
  def parse(location: Location): (Either[ParseError,A], Location)
}
case class ParserDecorator[A](
  underlying: MyParser[A],
  override val issueLevel: IssueLevel = IssueLevel.Warning,
  label: String = "",
  override val scopes: List[String] = Nil
) extends MyParser[A] {
  override def parse(location: Location): (Either[ParseError,A], Location) =
    underlying.parse(location)
}
case class StringParser(s: String) extends MyParser[String] {
  override def parse(location: Location): (Either[ParseError,String], Location) =
    if (location.currentLine.drop(location.col).startsWith(s))
      (Right(s), location.advanceBy(s.length))
    else
      (Left(location.toError(s"Input did not match $s.")), location)
}
case class OrParser[A](p1: MyParser[A], p2: () => MyParser[A]) extends MyParser[A] {
  override def parse(location: Location): (Either[ParseError,A], Location) =
    p1.parse(location) match {
      case x@(Right(_), _) => x
      case (Left(err1), _) => p2().parse(location) match {
        case y@(Right(_), _) => y
        case (Left(err2), _) =>
          val msg = s"""Tried two alternatives:
first result: $err1
second result: $err2"""
          (Left(location.toError(msg)), location)
      }
    }
}
case class ConsumedInput(p: MyParser[_]) extends MyParser[String] {
  override def parse(location: Location): (Either[ParseError,String], Location) =
    p.parse(location) match {
      case (Right(_), newLoc) =>
        val output = location
          .input
          .drop(location.offset)
          .take(newLoc.offset - location.offset)
        (Right(output), location)
      case (Left(err), _) => (Right(s"Error parsing input: $err."), location)
    }
}

object MyParsers extends Parsers[MyParser] {
  override def attempt[A](p: MyParser[A]): MyParser[A] = ???

  override def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???

  override def label[A](m: String)(p: MyParser[A]): MyParser[A] =
    p.setLabel(m)

  override def or[A](s1: MyParser[A],s2: => MyParser[A]): MyParser[A] =
    OrParser(s1, () => s2)

  override implicit def regex(r: scala.util.matching.Regex): MyParser[String] = ???

  override def run[A](p: MyParser[A])(input: String): Either[ParseError,A] =
    p.parse(Location(input, 0))._1

  override def scope[A](m: String)(p: MyParser[A]): MyParser[A] =
    p.pushScope(m)

  override def setIssueLevel[A](a: MyParser[A],e: IssueLevel): MyParser[A] =
    a.setIssueLevel(e)

  override def slice[A](p: MyParser[A]): MyParser[String] =
    ConsumedInput(p)

  override implicit def string(s: String): MyParser[String] =
    StringParser(s)
}
