package fpinscala
package applicative

import monads.Functor
import state._
import State._
import StateUtil._ // defined at bottom of this file
import monoids._
import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { self =>

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val g: F[A => (B => C)] = unit(a => f(a, _))
    apply(apply(g)(fa))(fb)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab, fa)(_(_))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] = fas match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] = as match {
    case Nil => unit(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = map2(unit(n), fa)(List.fill(_)(_))

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)(_ -> _)

  def map3[A,B,C,D](
    fa: F[A], fb: F[B], fc: F[C]
  )(
    f: (A, B, C) => D
  ): F[D] = apply(apply(apply(unit(f.curried))(fa))(fb))(fc)

  def map4[A,B,C,D,E](
    fa: F[A], fb: F[B], fc: F[C], fd: F[D]
  )(
    f: (A, B, C, D) => E
  ): F[E] = apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] =
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) =
        (self.unit(a), G.unit(a))
      override def map2[A,B,C](p0: (F[A], G[A]), p1: (F[B], G[B]))(f: (A,B) => C): (F[C],G[C]) =
        (self.map2(p0._1, p1._1)(f), G.map2(p0._2, p1._2)(f))
    }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] =
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def apply[A,B](fga2b: F[G[A => B]])(fga: F[G[A]]): F[G[B]] =
        self.map2(fga2b, fga) { (ga2b, ga) =>
          G.map2(ga2b, ga) { (a2b, a) =>
            a2b(a)
          }
        }
    }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    val fkv = ofa.iterator.map {
      case (k, fv) => map(fv)(k -> _)
    }
    map(sequence(fkv.toList))(_.toMap)
  }
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

trait exercise11[F[_]] extends Monad[F] {
  def compose[G[_]](G: Monad[G]): Monad[({type f[x] = F[G[x]]})#f] = {
    new Monad[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = ???
        // super.unit(G.unit(a)) doesn't compile - super.unit does not
        // accept a higher-kinded type.
      override def flatMap[A,B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]] = ???
        // super.flatMap(fga) { ga => G.flatMap(ga) { a => f(a) } }
        // doesn't compile either. G.flatMap requires a function that
        // returns a G[B], but f returns F[G[B]].
    }
  }
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[R](r: => R): Either[E,R] = Right(r)
      override def flatMap[R,S](x: Either[E,R])(f: R => Either[E,S]) = x match {
        case Left(e) => Left(e)
        case Right(r) => f(r)
      }
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] = ???
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E,x]})#f] {
      override def unit[A](a: => A): Validation[E,A] = Success(a)
      override def map2[A,B,C](
        ea: Validation[E,A],
        eb: Validation[E,B]
      )(
        f: (A, B) => C
      ): Validation[E,C] = (ea, eb) match {
        case (Success(a),      Success(b))      => Success(f(a, b))
        case (Failure(h, t),   Success(b))      => Failure(h, t)
        case (Success(a),      Failure(h, t))   => Failure(h, t)
        case (Failure(h0, t0), Failure(h1, t1)) => Failure(h0, t0 ++ (h1 +: t1))
      }
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  def map[A,B](fa: F[A])(f: A => B): F[B] = {
    implicit def G: Applicative[Option] = new Applicative[Option] {
      override def unit[T](t: => T) = Some(t)
      override def map2[T,U,V](gt: Option[T], gu: Option[U])(f: (T,U) => V) =
        gt.flatMap(t => gu.map(u => f(t, u)))
    }
    traverse(fa)(a => Some(f(a)): Option[B]).get
  }

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _  <- set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] = ???

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B = ???

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) = ???

  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] = ???
}

object Traverse {
  val listTraverse = new Traverse[List] {
    override def sequence[G[_]:Applicative,A](fma: List[G[A]]): G[List[A]] =
      fma match {
        case Nil => implicitly[Applicative[G]].unit(Nil)
        case h :: t =>
          implicitly[Applicative[G]].map2(h, sequence(t))(_ :: _)
      }
  }

  val optionTraverse = new Traverse[Option] {
    //def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    override def traverse[G[_]:Applicative,A,B](
      fa: Option[A]
    )(
      f: A => G[B]
    ): G[Option[B]] = fa match {
      case None => implicitly[Applicative[G]].unit(None)
      case Some(a) => implicitly[Applicative[G]].map(f(a))(Some(_))
    }
  }

  val treeTraverse = new Traverse[Tree] {
    //def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    override def traverse[G[_]:Applicative,A,B](
      fa: Tree[A]
    )(
      f: A => G[B]
    ): G[Tree[B]] = fa match {
      case Tree(h, t) =>
        val gList = implicitly[Applicative[G]].traverse(t)(traverse(_)(f))
        implicitly[Applicative[G]].map2(f(h), gList)(Tree(_, _))
    }
  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
