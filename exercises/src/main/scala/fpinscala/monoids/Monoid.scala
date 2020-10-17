package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 + a2
    val zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int) = a1 * a2
    val zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 || a2
    val zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean) = a1 && a2
    val zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
    val zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    def zero = identity
  }

  // TODO: Placeholder for `Prop`. Remove once you have implemented the `Prop`
  // data type from Part 2.
  //trait Prop {}

  // TODO: Placeholder for `Gen`. Remove once you have implemented the `Gen`
  // data type from Part 2.

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(gen)(a => m.op(m.zero, a) == a) &&
    Prop.forAll(gen)(a => m.op(a, m.zero) == a)
  }

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldRight[AA, BB](as: List[AA])(z: BB)(f: (AA, BB) => BB): BB = {
    type A = AA
    type B = BB => BB
    val g: A => B = aa => f(aa, _)
    val monoid = new Monoid[B] {
      def op(a1: B, a2: B) = a1 compose a2
      def zero = identity
    }
    foldMap(as, monoid)(g)(z)
  }

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
    val g: A => B => B = a => f(_, a)
    val monoid = new Monoid[B => B] {
      def op(a1: B => B, a2: B => B) = a1 andThen a2
      def zero = identity
    }
    foldMap(as, monoid)(g)(z)
  }

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if (as.length == 0) m.zero
    else if (as.length == 1) m.op(f(as.head), m.zero)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMap(
      ints.dropRight(1).toList.zip(ints.toList.drop(1)), booleanAnd
    )(
      p => p._1 < p._2
    )

  sealed trait WC
  case class Stub(chars: String) extends WC {
    require (chars.forall(_.isLetterOrDigit))
  }
  case class Part(lStub: String, words: Int, rStub: String) extends WC {
    require (lStub.forall(_.isLetterOrDigit) && rStub.forall(_.isLetterOrDigit))
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = 
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    val zero = Stub("")
    def op(a1: WC, a2: WC) = (a1, a2) match {
      case (Stub(s1), Stub(s2)) =>
        Console.err.println(s"Stub('$s1' + '$s2')")
        Stub(s1 + s2)
      case (Part(lstub, words, rstub), Stub(s2)) =>
        Console.err.println(s"Part('$lstub', $words, '$rstub' + '$s2')")
        Part(lstub, words, rstub + s2)
      case (Stub(s1), Part(lstub, words, rstub)) =>
        Console.err.println(s"Part('$s1' + '$lstub', $words, '$rstub')")
        Part(s1 + lstub, words, rstub)
      case (Part(lstub1, words1, rstub1), Part(lstub2, words2, rstub2)) =>
        val sum = if (rstub1.isEmpty && lstub2.isEmpty) words1 + words2
                  else 1 + words1 + words2
        Console.err.println(s"Part('$lstub1', $sum, '$rstub2')")
        Part(lstub1, sum, rstub2)
    }
  }

  def tokenize(s: String): List[String] =
    if (s.isEmpty) Nil
    else {
      val (firstWord, remaining) = s.span(_.isLetterOrDigit)
      val next = remaining.dropWhile(!_.isLetterOrDigit)
      if (remaining.isEmpty) List(firstWord)
      else if (firstWord.isEmpty) {
        "" :: tokenize(next)
      }
      else {
        if (next.isEmpty) List(firstWord, "")
        else firstWord :: tokenize(next)
      }
    }

  def divideAndConquer(s: String): WC = {
    val threshold = 4   // must be at least 2 for this code to work
    if (s.length <= threshold) {
      val tokens = tokenize(s)
      if (tokens.isEmpty) wcMonoid.zero
      else if (tokens.length == 1) Stub(tokens.head)
      else Part(tokens.head, tokens.length - 2, tokens.last)
    }
    else {
      val (s1, s2) = s.splitAt(s.length / 2)
      wcMonoid.op(divideAndConquer(s1), divideAndConquer(s2))
    }
  }

  def count(s: String): Int = divideAndConquer(s) match {
    case Stub(stub) if stub.isEmpty => 0
    case Stub(stub) => 1
    case Part(lstub, wordCount, rstub) =>
      if (lstub.isEmpty && rstub.isEmpty) wordCount
      else if (lstub.isEmpty || rstub.isEmpty) wordCount + 1
      else wordCount + 2
  }

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override val zero = (A.zero, B.zero)
      override def op(pair0: (A, B), pair1: (A, B)): (A, B) =
        (A.op(pair0._1, pair1._1), B.op(pair0._2, pair1._2))
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override val zero: A => B = _ => B.zero
      override def op(x: A => B, y: A => B): A => B = { a =>
        B.op(x(a), y(a))
      }
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      def zero = Map[K,V]()
      def op(a: Map[K, V], b: Map[K, V]) =
        (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
          acc.updated(k, V.op(a.getOrElse(k, V.zero),
                              b.getOrElse(k, V.zero)))
        }
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    as.map(a => Map(a -> 1)).reduce(mapMergeMonoid[A, Int](intAddition).op)
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldLeft(as)(z)((b, a) => f(a, b))
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldRight(as)(z)((a, b) => f(b, a))
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldMap(as)(identity)(m)
  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as match {
      case h :: t => f(h, foldRight(t)(z)(f))
      case Nil => z
    }
  @annotation.tailrec
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as match {
      case h :: t => foldLeft(t)(f(z, h))(f)
      case Nil => z
    }
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    foldMap(as.map(a => (b: B) => f(a, b)))(identity)(Monoid.endoMonoid)(z)
  @annotation.tailrec
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    if (as.isEmpty) z
    else foldLeft(as.tail)(f(z, as.head))(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    foldLeft(as.reverse)(z)((b, a) => f(a, b))
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    if (as.isEmpty) z
    else foldLeft(as.tail)(f(z, as.head))(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    as match {
      case Leaf(a) => f(a)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(Monoid.endoMonoid)(z)
}

object OptionFoldable extends Foldable[Option] {
  import Monoid._
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    as.map(f).getOrElse(mb.zero)
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    foldMap(as)(a => (b: B) => f(b, a))(endoMonoid)(z)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(endoMonoid)(z)
}
