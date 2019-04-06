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

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, ys) => ys 
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, ys) => Cons(h, ys)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_, ys) => 
      if (n > 1) drop(ys, n - 1)
      else ys
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, ys) =>
      if (f(x)) dropWhile(ys, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, ys) => Cons(x, init(ys))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil: List[B])((a, acc) => Cons(f(a), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight2(as, Nil: List[A]){ (a, acc) => 
    if (f(a))Cons(a, acc)
    else acc
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, a) => Cons(a, acc))
  
  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)((acc, a) => f(a, acc))

  def flatten[A](ll: List[List[A]]): List[A] = foldRight2(ll, Nil:List[A])((a, acc) => append2(a, acc)) 

  def addOne(l: List[Int]): List[Int] = foldRight2(l, Nil: List[Int])((a, acc) => Cons(a + 1, acc))

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, acc) => Cons(a.toString, acc))

  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight2(as, Nil: List[B]){(a, acc) => append2(f(a), acc) }

  def flatMap2[A,B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap2(as)((a) => if (f(a)) List(a) else Nil)

  def zip(l1: List[Int], l2: List[Int]): List[Int] = flatMap2(l1)(a => map(l2)(b => a + b))

  def addPairWise(l1: List[Int], l2: List[Int]): List[Int] = 
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addPairWise(xs, ys))
    }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = 
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Cons(x, xs), Cons(y, ys)) if (x == y) => startsWith(xs, ys)
    case _        => false
  }
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil 
    case _   if startsWith(sup, sub) => true
    case Cons(x, xs) => hasSubsequence(xs, sub)
  }

  implicit class ListOps[A](list: List[A]) {
    def drop(n: Int): List[A] = List.drop(list, n)
    def length: Int = List.length(list)
  }
}
