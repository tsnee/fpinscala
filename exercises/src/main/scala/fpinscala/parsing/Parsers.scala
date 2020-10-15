package fpinscala.parsing

import language.higherKinds
import language.implicitConversions
import util.matching.Regex

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
  def many[A](p: Parser[A]): Parser[List[A]] = attempt(map2(p, many(p))(_ :: _)) or succeed(Nil)
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
      _ <- char('[')
      j <- product(json, product(char(','), json).map(_._2).many).map {
        case (hd, tl) => (hd :: tl).toIndexedSeq
      }
      _ <- char(']')
    } yield JArray(j)
    def keyValuePair = for {
      k <- jString
      _ <- char(':')
      _ <- spaces
      v <- json
    } yield k.get -> v
    def jObject: Parser[JSON] = for {
      _ <- char('{')
      keyValuePairs <- product(keyValuePair, product(char(','), keyValuePair).map(_._2).many).map { case (hd, tl) => (hd :: tl) }
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

// Exercise 12
trait MyParser[+A] {
  def setIssueLevel(issueLevel: IssueLevel): MyParser[A] =
    ParserDecorator(underlying = this, issueLevel = issueLevel)
  def setLabel(label: String): MyParser[A] =
    ParserDecorator(underlying = this, label = Some(label))
  def pushScope(scope: String): MyParser[A] =
    ParserDecorator(underlying = this, scope = Some(scope))
  def parse(location: Location, issueLevel: IssueLevel = IssueLevel.Warning, label: Option[String] = None, scope: Option[String] = None): (Either[ParseError,A], Location)
  def parse(input: String): (Either[ParseError,A], Location) = parse(Location(input, 0))
  : (Either[ParseError,A], Location)
  def flatMap[B](f: A => MyParser[B]): MyParser[B] = MyParsers.flatMap(this)(f)
  def determinePrefix(label: Option[String], scope: Option[String]): String = {
    val labelPrefix = label match {
      case Some(l) => s"$l: "
      case None => ""
    }
    val scopePrefix = scope match {
      case Some(s) => s"$s: "
      case None => ""
    }
    scopePrefix + labelPrefix
  }
}
case class Attempt[A](p: MyParser[A]) extends MyParser[A] {
  override def parse(location: Location, issueLevel: IssueLevel, label: Option[String], scope: Option[String])
  : (Either[ParseError,A], Location) =
    p.parse(location, issueLevel, label, scope)
}
case class ParserDecorator[A](
  underlying: MyParser[A],
  issueLevel: IssueLevel = IssueLevel.Warning,
  label: Option[String] = None,
  scope: Option[String] = None
) extends MyParser[A] {
  override def parse(location: Location, issueLevel: IssueLevel, incomingLabel: Option[String], incomingScope: Option[String])
  : (Either[ParseError,A], Location) =
    underlying.parse(location, issueLevel, if (label.isDefined) label else incomingLabel, if (scope.isDefined) scope else incomingScope)
}
case class StringParser(s: String) extends MyParser[String] {
  override def parse(location: Location, issueLevel: IssueLevel, label: Option[String], scope: Option[String])
  : (Either[ParseError,String], Location) =
    if (location.currentLine.drop(location.col).startsWith(s))
      (Right(s), location.advanceBy(s.length))
    else {
      val prefix = determinePrefix(label, scope)
      (Left(location.toError(s"${prefix}Input did not match $s.")), location)
    }
}
case class OrParser[A](p1: MyParser[A], p2: () => MyParser[A]) extends MyParser[A] {
  override def parse(location: Location, issueLevel: IssueLevel, label: Option[String], scope: Option[String])
  : (Either[ParseError,A], Location) =
    p1 match {
      case Attempt(p) => p.parse(location, issueLevel, label, scope) match {
        case x@(Right(_), _) => x
        case (Left(err1), _) => p2().parse(location, issueLevel, label, scope) match {
          case y@(Right(_), _) => y
          case (Left(err2), _) =>
            val prefix = determinePrefix(label, scope)
            val msg = s"""${prefix}Tried two alternatives:
first result: $err1
second result: $err2"""
            (Left(location.toError(msg)), location)
        }
      }
      case _ => p1.parse(location, issueLevel, label, scope)
    }
}
case class ConsumedInput(p: MyParser[_]) extends MyParser[String] {
  override def parse(location: Location, issueLevel: IssueLevel, label: Option[String], scope: Option[String])
  : (Either[ParseError,String], Location) =
    p.parse(location, issueLevel, label, scope) match {
      case (Right(_), newLoc) =>
        val output = location
          .input
          .drop(location.offset)
          .take(newLoc.offset - location.offset)
        (Right(output), location)
      case (Left(err), _) =>
        val prefix = determinePrefix(label, scope)
        (Right(s"${prefix}Error parsing input: $err."), location)
    }
}
case class RegexParser(r: Regex) extends MyParser[String] {
  override def parse(location: Location, issueLevel: IssueLevel, label: Option[String], scope: Option[String])
  : (Either[ParseError,String], Location) =
    r.findPrefixOf(location.currentLine.drop(location.col)) match {
      case Some(s) => (Right(s), location.advanceBy(s.length))
      case None =>
        val prefix = determinePrefix(label, scope)
        (Left(location.toError(s"${prefix}Input did not match $r.")), location)
    }
}
case class FlatMapParser[A,B](p: MyParser[A], f: A => MyParser[B]) extends MyParser[B] {
  override def parse(location: Location, issueLevel: IssueLevel, label: Option[String], scope: Option[String])
  : (Either[ParseError,B], Location) =
    p.parse(location, issueLevel, label, scope) match {
      case (Right(a), newLoc) => f(a).parse(newLoc, issueLevel, label, scope)
      case (Left(err), _) => (Left(err), location)
    }
}

object MyParsers extends Parsers[MyParser] {
  override def attempt[A](p: MyParser[A]): MyParser[A] = Attempt(p)

  override def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = FlatMapParser(p, f)

  override def label[A](m: String)(p: MyParser[A]): MyParser[A] = p.setLabel(m)

  override def or[A](s1: MyParser[A],s2: => MyParser[A]): MyParser[A] = OrParser(s1, () => s2)

  override implicit def regex(r: scala.util.matching.Regex): MyParser[String] = RegexParser(r)

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

object Exercise_13 {
  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  implicit def string(s: String): Parser[String] = { location =>
    if (location.currentLine.drop(location.col).startsWith(s))
      Success(s, s.length)
    else
      Failure(location.toError(s"Input did not match $s."))
  }

  implicit def regex(r: Regex): Parser[String] = { location =>
    r.findPrefixOf(location.currentLine.drop(location.col)) match {
      case Some(s) => Success(s, s.length)
      case None => Failure(location.toError(s"Input did not match $r."))
    }
  }

  def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

  def slice[A](p: Parser[A]): Parser[String] = { location =>
    p(location) match {
      case Success(_, consumed) => Success(location.input.slice(location.offset, location.offset + consumed), consumed)
      case Failure(err) => Failure(err)
    }
  }

  def run[A](p: Parser[A])(input: String): Either[ParseError,A] = p(Location(input)) match {
    case Success(a, _) => Right(a)
    case Failure(err) => Left(err)
  }
}

object Exercise_14 {
  type Parser[+A] = Location => Result[A]

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

  case class ParseError(
    stack: List[(Location,String)] = List(),
    otherFailures: List[ParseError] = List()
  ) {
    def push(loc: Location, msg: String): ParseError = copy(stack = (loc,msg) :: stack)
    def label[A](s: String): ParseError = ParseError(latestLoc.map((_,s)).toList)
    def latestLoc: Option[Location] = latest map (_._1)
    def latest: Option[(Location,String)] = stack.lastOption
  }

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e) => Failure(f(e))
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

  def scope[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.push(s,msg))
  def label[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.label(msg))

  implicit def string(s: String): Parser[String] = { location =>
    if (location.currentLine.drop(location.col).startsWith(s))
      Success(s, s.length)
    else
      Failure(location.toError(s"Input did not match $s."))
  }
}

object Exercise_15 {
  type Parser[+A] = Location => Result[A]

  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

    def toError(msg: String): ParseError =
      ParseError(List((this, msg)))

    def advanceBy(n: Int): Location = copy(offset = offset+n)

    /* Returns the line corresponding to this location */
    def currentLine: String = 
      if (input.length > 1) input.lines.drop(line-1).next
      else ""
  }

  case class ParseError(
    stack: List[(Location,String)] = List(),
    otherFailures: List[ParseError] = List()
  ) {
    def push(loc: Location, msg: String): ParseError = copy(stack = (loc,msg) :: stack)
    def label[A](s: String): ParseError = ParseError(latestLoc.map((_,s)).toList)
    def latestLoc: Option[Location] = latest map (_._1)
    def latest: Option[(Location,String)] = stack.lastOption
  }

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, committed) => Failure(f(e), committed)
      case _ => this
    }
    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }
    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _ => this
    }
    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  object MyParser extends Parsers[Parser] {
    override def scope[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.push(s,msg))

    override def label[A](msg: String)(p: Parser[A]): Parser[A] = s => p(s).mapError(_.label(msg))

    implicit override def string(s: String): Parser[String] = { location =>
      val input = location.input.drop(location.offset).take(s.length)
      if (input == s)
        Success(s, s.length)
      else
        Failure(location.toError(s"Expected '$s' but found '$input'."), true)
    }

    implicit override def regex(r: Regex): Parser[String] = { location =>
      r.findPrefixOf(location.currentLine.drop(location.col)) match {
        case Some(s) => Success(s, s.length)
        case None => Failure(location.toError(s"Input did not match $r."), true)
      }
    }

    override def succeed[A](a: A): Parser[A] = _ => Success(a, 0)

    override def slice[A](p: Parser[A]): Parser[String] = { location =>
      p(location) match {
        case Success(_, consumed) => Success(location.input.slice(location.offset, location.offset + consumed), consumed)
        case Failure(err, committed) => Failure(err, committed)
      }
    }

    override def attempt[A](p: Parser[A]): Parser[A] = s => p(s).uncommit

    override def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = s => x(s) match {
      case Failure(e,false) => y(s)
      case r => r
    }

    override def flatMap[A,B](f: Parser[A])(g: A => Parser[B]): Parser[B] = s => f(s) match {
      case Success(a,n) => g(a)(s.advanceBy(n)).addCommit(n != 0).advanceSuccess(n)
      case e@Failure(_,_) => e
    }

    override def setIssueLevel[A](a: Parser[A], e: IssueLevel): Parser[A] = ???

    override def run[A](p: Parser[A])(input: String): Either[fpinscala.parsing.ParseError,A] = p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(err, _) => Left(err)
    }

    implicit private def new2oldParseError(pe: ParseError): fpinscala.parsing.ParseError =
      fpinscala.parsing.ParseError(
        pe.stack.map { case (loc, str) => (new2oldLocation(loc), str) },
        pe.otherFailures.map(new2oldParseError))

    implicit private def new2oldLocation(loc: Location): fpinscala.parsing.Location = Location(loc.input, loc.offset)
  }
}
