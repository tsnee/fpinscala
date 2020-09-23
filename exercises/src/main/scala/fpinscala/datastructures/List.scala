package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => sys.error("Cannot replace head of empty list.")
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0, "Numeric argument to drop must be nonnegative.")
    @annotation.tailrec
    def loop(list: List[A], counter: Int): List[A] =
      list match {
        case Nil => Nil
        case Cons(_, t) =>
          if (counter > 0) {
            loop(t, counter - 1)
          } else {
            t
          }
      }
    loop(l, n)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  def init[A](l: List[A]): List[A] = {
    def loop(oldHead: A, list: List[A]): List[A] = list match {
      case Cons(h, t) => Cons(oldHead, loop(h, t))
      case Nil => Nil
    }
    l match {
      case Cons(h, t) => loop(h, t)
      case Nil => Nil
    }
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum11(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)
  def product11(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)
  def length11[A](ns: List[A]): Int = foldLeft(ns, 0)((b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A]) { (acc, x) =>
    Cons(x, acc)
  }

  def foldRight13[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft13[A,B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a, b) => f(b, a))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((b, a) => Cons(a, b))

  def flatten[A](as: List[List[A]]): List[A] = foldRight(as, Nil: List[A])((a, b) => append(a, b))

  def plusOne(xs: List[Int]): List[Int] = xs match {
    case Cons(h, t) => Cons(h + 1, plusOne(t))
    case Nil => Nil
  }

  def doubleToString(xs: List[Double]): List[String] = xs match {
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
    case Nil => Nil
  }

  def doubleToString2(xs: List[Double]): List[String] = foldRight(xs, Nil: List[String])((a, b) => Cons(a.toString, b))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Cons(h, t) => Cons(f(h), map(t)(f))
    case Nil => Nil
  }

  def mapAlso[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((a, b) => Cons(f(a), b))

  def filter[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
    case Cons(h, t) if p(h) => Cons(h, filter(t)(p))
    case Cons(h, t) => filter(t)(p)
    case Nil => Nil
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))
  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Cons(h, t) => append(f(h), flatMap2(t)(f))
    case Nil => Nil
  }

  def filterViaFlatMap[A](xs: List[A])(p: A => Boolean): List[A] = flatMap(xs)(x => if (p(x)) List(x) else Nil)

  def zipPlus(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(xh + yh, zipPlus(xt, yt))
    case _ => Nil
  }

  def zipWith[A,B,C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh, yh), zipWith(xt, yt)(f))
    case _ => Nil
  }

  def hasSubsequence[A](xs: List[A], ys: List[A]): Boolean = {
    @annotation.tailrec
    def loop(sup: List[A], sub: List[A], matching: Boolean): Boolean =
      (sup, sub) match {
        case (Cons(xh, xt), Cons(yh, yt)) if xh == yh => loop(xt, yt, true)
        case (Cons(xh, xt), Cons(yh, yt)) if matching => loop(sup, ys, false)
        case (Cons(xh, xt), Cons(yh, yt)) => loop(xt, ys, false)
        case (_, Nil) => matching
        case (Nil, _) => false
      }
    loop(xs, ys, false)
  }
}
