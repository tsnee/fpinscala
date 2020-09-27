package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng0: RNG): (Int, RNG) = rng0.nextInt match {
    case t@(n, _) if n >= 0 => t
    case (n, rng1) if n == Int.MinValue => (0, rng1)
    case (n, rng1) if n < 0 => (-n, rng1)
  }

  def double(rng0: RNG): (Double, RNG) = nonNegativeInt(rng0) match {
    case (i, rng1) => (i.toDouble / (Int.MaxValue.toDouble + 1.0), rng1)
  }

  def intDouble(rng0: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = rng0.nextInt
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def doubleInt(rng0: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng0)
    val (i, rng2) = rng1.nextInt
    ((d, i), rng2)
  }

  def double3(rng0: RNG): ((Double,Double,Double), RNG) = {
    val (d0, rng1) = double(rng0)
    val (d1, rng2) = double(rng1)
    val (d2, rng3) = double(rng2)
    ((d0, d1, d2), rng3)
  }

  def ints(count: Int)(rng0: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(size: Int, r0: RNG, l: List[Int]): (List[Int], RNG) =
      if (size < 1) {
        (l, r0)
      } else {
        val (i, r1) = r0.nextInt
        loop(size - 1, r1, i :: l)
      }
    loop(count, rng0, Nil)
  }

  def double2: Rand[Double] = map(nonNegativeInt)(_.toDouble / (Int.MaxValue.toDouble + 1.0))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = { rng0 =>
    val (a, rng1) = ra(rng0)
    val (b, rng2) = rb(rng1)
    (f(a, b), rng2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng0 =>
    fs.foldRight((List.empty[A], rng0)) {
      case (f, (l, r0)) =>
        val (a, r1) = f(r0)
        (a :: l, r1)
    }
  }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng0 =>
    val (a, rng1) = f(rng0)
    g(a)(rng1)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod)
    else nonNegativeLessThan(n)
  }

  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    map(rb) { b =>
      f(a, b)
    }
  }
}

case class State[S,+A](run: S => (A, S)) {
  import State.unit
  def flatMap[B](f: A => State[S, B]): State[S, B] = State { s0 =>
    val (a, s1) = run(s0)
    f(a).run(s1)
  }
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldRight(unit(Nil): State[S,List[A]])((s, acc) => s.map2(acc)(_ :: _))
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State { machine =>
    val simulation = inputs.foldLeft(machine) {
      case (m, Coin) if m.locked && m.candies > 0 => m.copy(locked = false, coins = m.coins + 1)
      case (m, Turn) if !m.locked => m.copy(locked = true, candies = m.candies - 1)
      case (m, Turn) if m.locked => m
      case (m, Coin) if !m.locked => m
      case (m, _) if (m.candies == 0) => m
    }
    ((simulation.coins, simulation.candies), simulation)
  }
}
