package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((a, b) => {
    lazy val b1 = b // tuple 
    val b2 = f(a, b1._1) // state
    (b2, cons(b2, b1._2))
  })._2

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => cons(h(), t() take (n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Empty                => empty
    case Cons(h, t) => 
      if (n > 1) t() drop (n -1)
      else t()
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t () takeWhile p)
    case Cons(h, t)           => cons(h(), empty)
    case _                    => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, acc) => if (p(a)) cons(a, acc) else empty)

  def forAll(p: A => Boolean): Boolean = foldRight(false)((a, b) => if (p(a)) b else false)

  def headOption: Option[A] = foldRight(Option.empty[A])((a, b) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))
  def append[B >: A](s2: Stream[B]): Stream[B] = foldRight(s2)(cons(_, _) )
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

   
  // stackoverflow prone
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def saveToList: List[A] = {
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => loop(t(), acc :+ h())
    }

    loop(this, Nil)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None        => empty
  }

  def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this){
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeWithUnFold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, _), 1) if n > 1 => Some((h(), (empty, 0)))
    case (Cons(h, t), nn) if nn > 1 => Some((h(), (t(), n -1)))
    case _                    => None
  }

  def takeWhileWithUnFold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), (t())))
    case Cons(h, t)           => Some((h(), (empty)))
    case _                         => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h, t), Cons(h1, t1)) => Some((f(h(), h1()), (t(), t1())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h, t), Cons(h1, t1)) => Some(((Some(h()), Some(h1())), (t(), t1())))
    case (empty, Cons(h1, t1))      => Some(((None, Some(h1())), (empty, t1())))
    case (Cons(h, t), empty)        => Some(((Some(h()), None), (t(), empty)))
    case _                          => None
  }

  def startsWithEasy[B](s: Stream[B]): Boolean = (unfold((this, s)){
    case (Cons(h, t), Cons(h1, t1)) if (h() == h1()) => Some(((true), (t(), t1())))
    case _          => None
  }).headOption.getOrElse(false)

  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).takeWhile2(!_._2.isEmpty).forAll{
    case (h, h1) => h == h1
  }

  def tails: Stream[Stream[A]] = unfold(this){
    case (Cons(h, t)) => Some(((cons(h(), t())), (t())))
    case _            => None
  }


  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)
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

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  private def _fibs(prev: Int , cur: Int): Stream[Int] = Stream.cons(prev, _fibs(cur, prev + cur))
  val fibs = _fibs(0, 1)


  def onesWithUnfold: Stream[Int] = empty.unfold(1)((a) => Some(a, 1))
  def fromWithUnfold(n: Int): Stream[Int] = empty.unfold(n)((s) => Some(s, s + 1))
  def constantWithUnfold[A](a: A): Stream[A] = empty.unfold(a)((s) => Some(s, s))
  def fibsWithUnfold: Stream[Int] = empty.unfold((0, 1))((s) => Some(s._1, (s._2, s._1 + s._2)))

}
