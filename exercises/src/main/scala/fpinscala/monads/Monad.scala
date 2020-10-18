package fpinscala
package monads

import parsing._
import testing._
import parallelism._
import state._
import parallelism.Par._
import language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(identity)

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight(unit(List.empty[B])) { (a, acc) => map2(f(a), acc)(_ :: _) }

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))

  def filterM[A](ms: List[A])(f: A => M[Boolean]): M[List[A]] =
    map(sequence(ms.map(f)))(bs => bs.zip(ms).filter(_._1).map(_._2))

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] = a => flatMap(f(a))(g)

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = compose(identity: M[A] => M[A], f)(ma)

  //Exercise 9
  // flatMap(flatMap(x)(f))(g) == flatMap(x)(a => flatMap(f(a))(g))
  // flatMap(x)(f) = compose(identity, f)(x)
  // flatMap(compose(identity, f)(x))(g) == flatMap(x)(a => flatMap(f(a))(g))
  // compose(identity, g)(compose(identity, f)(x)) == flatMap(x)(a => flatMap(f(a))(g))
  // flatMap(f(a))(g) = compose(identity, g)(f(a))
  // compose(identity, g)(compose(identity, f)(x)) == flatMap(x)(a => compose(identity, g)(f(a)))
  // compose(identity, g)(compose(identity, f)(x)) == compose(identity, a => compose(identity, g)(f(a)))(x)

  //Exercise 10
  // Left identity:
  // compose(f, unit) == f
  // a => flatMap(f(a))(unit) == a => f(a)
  // flatMap(x)(unit) == x
  // x == x
  // Right identity:
  // compose(unit, f) == f
  // a => flatMap(unit(a))(f) == a => f(a)
  // a => f(a) == a => f(a)
  // x == x

  //Exercise 11
  // Left identity for Option:
  // flatMap(x)(unit) == x
  // flatMap(x)(Some) == x
  // x = Some(v)
  // flatMap(Some(v))(Some) == Some(v)
  // Some(v) == Some(v)
  // Right identity for Option:
  // flatMap(unit(y))(f) == f(y)
  // flatMap(Some(y))(f) == f(y)
  // f(y) == f(y)

  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A,B](ma: Par[A])(f: A => Par[B]): Par[B] =
      choiceGeneral(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new Monad[P] {
    override def unit[A](a: => A): P[A] = p.succeed(a)
    override def flatMap[A,B](ma: P[A])(f: A => P[B]): P[B] =
      p.flatMap(ma)(f)
  }

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)
    override def flatMap[A,B](ma: Option[A])(f: A => Option[B]): Option[B] =
      ma flatMap f
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = Stream(a)
    override def flatMap[A,B](ma: Stream[A])(f: A => Stream[B]): Stream[B] =
      ma flatMap f
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] =
      ma flatMap f
  }

  type RngState[A] = State[RNG,A]
  def stateMonad[S] = new Monad[RngState] {
    override def unit[A](a: => A): RngState[A] = State.unit(a)
    override def flatMap[A,B](ma: RngState[A])(f: A => RngState[B]): RngState[B] =
      ma flatMap f
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A,B](ma: Id[A])(f: A => Id[B]): Id[B] = f(ma.value)
  }

  def readerMonad[R] = Reader.readerMonad[R]
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = Reader(_ => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] = Reader(r => f(st.run(r)).run(r))
  }
}
// Primitive operations: run, unit, flatMap.
// flatMap runs two consecutive actions with the same input R, where the second action is created by the first.
// sequence takes a list of actions and runs them all, in order, with the same input R.
// join takes a task within a task and returns a task.
// replicateM takes a number n and a task t and returns a task that returns a List of results of running t n times.
// The associative law means that the result of a series of tasks will be the same no matter how the tasks are grouped together
// as long as they are run in the same order.
// The identity laws mean that a task can be run before or after an identity task and still return the same result.
