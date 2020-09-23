package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size(t: Tree[_]): Int = t match {
    case Branch(left: Tree[_], right: Tree[_]) => 1 + size(left) + size(right)
    case Leaf(_) => 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Branch(left: Tree[Int], right: Tree[Int]) => maximum(left) max maximum(right)
    case Leaf(v) => v
  }

  def depth(t: Tree[_]): Int = t match {
    case Branch(left: Tree[_], right: Tree[_]) => 1 + (depth(left) max depth(right))
    case Leaf(_) => 0
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Branch(left: Tree[A], right: Tree[A]) => Branch(map(left)(f), map(right)(f))
    case Leaf(v) => Leaf(f(v))
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Branch(left: Tree[A], right: Tree[A]) => g(fold(left)(f)(g), fold(right)(f)(g))
    case Leaf(v) => f(v)
  }

  def sizeViaFold(t: Tree[_]): Int = fold(t)(_ => 1)(_ + _ + 1)

  def maximumViaFold(t: Tree[Int]): Int = fold(t)(identity)(_ max _)

  def depthViaFold(t: Tree[_]): Int = fold(t)(_ => 0)((x: Int, y: Int) => (x max y) + 1)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}
