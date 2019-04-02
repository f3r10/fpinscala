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

  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // def nonNegativeInt(rng: RNG): (Int, RNG) = {
  def nonNegativeInt: Rand[Int] = rng => {
    val (i1, rng2) = rng.nextInt
    (if (i1 < 0) -(i1 + 1) else i1, rng2)
  }

  def doubleWithMap(rng: RNG): Rand[Double] = 
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def double(rng: RNG): (Double, RNG) = {
    val (i1, rng2) = nonNegativeInt(rng)
    (i1 / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (d1, rng3) = double(rng2)
    ((i1, d1), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (i1, rng3) = rng2.nextInt
    ((d1, i1), rng3)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(n: Int, acc: List[Int], rngN: RNG): (List[Int], RNG) =
      if (n == 0) (acc, rngN)
      else {
        val (i, newRng) = rngN.nextInt
        loop(n-1, i :: acc, newRng)
      } 
    loop(count, List.empty, rng)
  }

  def intsWitnSequence(count: Int): Rand[List[Int]] =
    _sequence(List.fill(count)(int))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a,b)))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rngN) = ra(rng) 
    val (b, rngN2) = rb(rngN)
    ((f(a,b)), rngN2)
  }

  def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs match {
      case Nil => (Nil, rng)
      case x :: xs => map2(x, _sequence(xs))((a, b) => a :: b)(rng)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit(List[A]()))((a, acc) => map2(a, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(i => unit(f(i)))

  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt){ state =>
    val mod = state % n
    if (state + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }
}

case class State[S,+A](run: S => (A, S)) {
  // def map[B](f: A => B): State[S, B] = State(s => {
  //   val (a, ns) = run(s)
  //   (f(a), ns)
  // })
  def map[B](f: A => B): State[S, B] = this.flatMap(i => State.unit(f(i)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = this.flatMap(a => sb.map(b => f(a,b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, ns) = run(s)
    f(a).run(ns)
   } 
  )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {
  import State._

  def update = (i:Input) => (m: Machine) => 
    (i, m) match {
      case (_, Machine(_, 0, _)) => m
      case (Coin, Machine(false, _, _)) => m
      case (Turn, Machine(true, _, _)) => m
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }//:: input -> machine -> machine

    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
     val step1 = sequence(inputs map { input => 
        //:: machine -> machine -> State[Machine, Unit]
        // ____^_______
        modify[Machine](update(input)): State[Machine, Unit]
      }: List[State[Machine, Unit]]): State[Machine, List[Unit]]
     //             Unit
    //              ___^___
     step1.flatMap{ _ => 
       //change Unit:A to (Int, Int):B
       //____________^___________________
       get1.map(m => (m.coins, m.candies))
     }
    }
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  type Rand[A] = State[RNG, A]


  // stack overflow prone
  private def _sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List[A]()))((a, acc) => a.map2(acc)(_ :: _))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[S, List[A]](List[A]()))((acc, a) => a.map2(acc)(_ :: _))

  def get1[S]: State[S, S] = State(s => (s,s))

  def set1[S](s: S): State[S, Unit] = State(_ => ((), s))

  def _modify[S](f: S => S): State[S, Unit] = 
    get1.flatMap(s => set1(f(s)))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get1
    _ <- set1(f(s))
  } yield ()
}
