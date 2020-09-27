package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Stream.empty[A]
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(h, t) => this
    case Empty => Stream.empty[A]
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty[A]
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else b)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((h, t) => Stream.cons(f(h), t))
  def filter(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((h, t) => if (p(h)) Cons(() => h, () => t) else t)
  def append[AA >: A](s: => Stream[AA]): Stream[AA] = foldRight(s)((h, t) => Stream.cons(h, t))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((h, t) => f(h).append(t))

  def mapViaUnfold[B](f: A => B): Stream[B] = Stream.unfold(this)(s => s.headOption.map(a => f(a) -> s.drop(1)))
  def takeViaUnfold(n: Int): Stream[A] = Stream.unfold((this, 0)) {
    case (Cons(h, t), idx) if idx < n => Some((h(), (t(), idx + 1)))
    case _ => None
  }
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }
  def zipWith[B, C](bs: => Stream[B])(f: (A, => B) => C): Stream[C] = Stream.unfold((this, bs)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((f(ha(), hb()), (ta(), tb())))
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this, s2)) {
    case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
    case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
    case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
    case _ => None
  }

  def startsWith[AA >: A](s: Stream[AA]): Boolean =
    zipAll(s).takeWhile(_._2.isDefined).forAll(t => t._1 == t._2)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this)(s => s.headOption.map(_ => (s, s.drop(1)))).append(Stream(Empty))

  def scanRightOrderNSquared[B](z: B)(f: (A, => B) => B): Stream[B] =
    Stream.unfold((this, z)) {
      case (stream: Cons[A], acc) =>
        val v = stream.foldRight(acc)(f)
        Some((v, (stream.drop(1), acc)))
      case _ => None
    } append Stream(z)

  def scanRight2N[B](z: B)(f: (A, => B) => B): Stream[B] = {
    val buf = collection.mutable.Buffer[A]()
    @annotation.tailrec
    def loop(s: Stream[A]): Unit = s match {
      case Cons(h, t) => buf.append(h()); loop(t())
      case Empty =>
    }
    loop(this)
    buf.foldRight((z, Stream(z))) {
      case (a, (b, stream)) => val v = f(a, b); (v, Cons(() => v, () => stream))
    }._2
  }

  def scanRightStackUnsafe[B](z: B)(f: (A, => B) => B): Stream[B] = {
    def loop(s: Stream[A]): (B, Stream[B]) = s match {
      case Cons(h, t) =>
        val (b, tail) = loop(t())
        val v = f(h(), b)
        (v, Cons(() => v, () => tail))
      case Empty => (z, Stream(z))
    }
    loop(this)._2
  }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z))) {
    case (a, (b, stream)) => val v = f(a, b); (v, Cons(() => v, () => stream))
  }._2
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = { //Stream.cons(a, constant(a))
    lazy val tail: Stream[A] = Stream.cons(a, tail)
    tail
  }

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def loop(i: Int, j: => Int): Stream[Int] = Stream.cons(i, loop(j, i + j))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty
    }

  val fibsViaUnfold: Stream[Int] = unfold((0, 1))(t => Some(t._1, (t._2, t._1 + t._2)))

  def fromViaUnforld(n: Int): Stream[Int] = unfold(n)(x => Some((x, x + 1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))

  val onesViaUnfold: Stream[Int] = unfold(1)(x => Some((x, x)))
}
