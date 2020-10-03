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

case class Prop9(run: (TestCases,RNG) => Result) {
  def check: Boolean
  def &&(p: => Prop9): Prop9 = new Prop9 {
  }
  def ||(p: => Prop9): Prop9 = new Prop9 {
  }
}
object Prop9 {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  def unit[A](a: => A): Gen[A] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
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

case class Gen8[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen8[B]): Gen8[B] =
    Gen8(sample.flatMap(a => f(a).sample))
  def listOfN[A](n: Int, g: Gen8[A]): Gen8[List[A]] =
    Gen8(State.sequence(List.fill(n)(g.sample)))
}
object Gen8 {
  def unit[A](a: => A): Gen8[A] = Gen8(State.unit(a))
  def boolean: Gen8[Boolean] =
    Gen8(State(RNG.map(RNG.int)(_ % 2 == 0)))
  def union[A](g1: Gen8[A], g2: Gen8[A]): Gen8[A] =
    boolean.flatMap(if (_) g1 else g2)
  def weighted[A](g1: (Gen8[A],Double), g2: (Gen8[A],Double)): Gen8[A] =
    Gen8(State(RNG.map(RNG.double2)(_ < g1._2 / (g1._2 + g2._2)))).flatMap { b =>
      if (b) g1._1 else g2._1
    }
}

trait SGen[+A] {

}
