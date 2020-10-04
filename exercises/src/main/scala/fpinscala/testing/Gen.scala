package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop3 {
  def check: Boolean
  def &&(p: => Prop3): Prop3 = new Prop3 {
    override def check: Boolean = this.check && p.check
    // Does "this" mean the anonynous inner class or the enclosing trait?
    // Answer: inner class, meaning this fails with infinite recursion.
    // Should be Prop3.this.check
  }
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (TestCases,RNG) => Result) {
  def &&(p: => Prop): Prop = Prop { (testCases,rng) =>
    run(testCases, rng) match {
      case Passed => p.run(testCases, rng)
      case f => f
    }
  }
  def ||(p: => Prop): Prop = Prop { (testCases,rng) =>
    run(testCases, rng) match {
      case Passed => Passed
      case Falsified(_, _) => p.run(testCases, rng)
    }
  }
}
object Prop {
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"""test case: $s
generated an exception: ${e.getMessage}
stack trace:
${e.getStackTrace.mkString("\n")}"""
}

case class Gen4[A](sample: State[RNG,A])
object Gen4 {
  def choose(start: Int, stopExclusive: Int): Gen4[Int] = Gen4 { 
    State { rng0 =>
      val (i, rng1) = RNG.nonNegativeLessThan(stopExclusive - start)(rng0)
      (i + start, rng1)
    }
  }
  def chooseBetter(start: Int, stopExclusive: Int): Gen4[Int] =
    Gen4(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
}

case class Gen5[A](sample: State[RNG,A])
object Gen5 {
  def unit[A](a: => A): Gen5[A] = Gen5(State(RNG.unit(a)))
  def boolean: Gen5[Boolean] =
    Gen5(State(RNG.map(RNG.int)(_ % 2 == 0)))
  def listOfN[A](n: Int, g: Gen5[A]): Gen5[List[A]] =
    Gen5(State(RNG.sequence(List.fill(n)(g.sample.run))))
}

case class Gen6[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen6[B]): Gen6[B] =
    Gen6(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen6[Int]): Gen6[List[A]] =
    size.flatMap(n => Gen6(sample.map(a => List.fill(n)(a))))
}
object Gen6 {
  def unit[A](a: => A): Gen6[A] = Gen6(State(RNG.unit(a)))
  def boolean: Gen6[Boolean] =
    Gen6(State(RNG.map(RNG.int)(_ % 2 == 0)))
  def listOfN[A](n: Int, g: Gen6[A]): Gen6[List[A]] =
    Gen6(State(RNG.sequence(List.fill(n)(g.sample.run))))
}

case class Gen7[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen7[B]): Gen7[B] =
    Gen7(sample.flatMap(a => f(a).sample))
}
object Gen7 {
  def unit[A](a: => A): Gen7[A] = Gen7(State.unit(a))
  def boolean: Gen7[Boolean] =
    Gen7(State(RNG.map(RNG.int)(_ % 2 == 0)))
  def union[A](g1: Gen7[A], g2: Gen7[A]): Gen7[A] =
    boolean.flatMap(if (_) g1 else g2)
}

// Answer to #8
case class Gen[+A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))
  def unsized: SGen[A] = SGen(_ => this)
  def listOf(n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(sample)))
  def listOf1(n: Int): Gen[List[A]] = {
    require(n > 0)
    Gen(State.sequence(List.fill(n)(sample)))
  }
}
object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeLessThan(stopExclusive - start)).map(_ + start))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] =
    Gen(State(RNG.map(RNG.int)(_ % 2 == 0)))
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)
  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] =
    Gen(State(RNG.map(RNG.double2)(_ < g1._2 / (g1._2 + g2._2)))).flatMap { b =>
      if (b) g1._1 else g2._1
    }
}

case class SGen[+A](forSize: Int => Gen[A])
object SGen {
  def unit[A](a: => A): SGen[A] = Gen.unit(a).unsized
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => Gen.listOfN(n, g))
}

object _14 {
  //def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
  val sortedProp = Prop.forAll(Gen.choose(-10, 10).listOf(10)) { l =>
    val sorted = l.sorted
    sorted.head == sorted.min
    sorted.last == sorted.max
  }
}
