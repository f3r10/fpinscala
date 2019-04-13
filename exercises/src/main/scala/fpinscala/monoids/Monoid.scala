package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.parallelism.Nonblocking.Par.toParOps // infix syntax for `Par.map`, `Par.flatMap`, etc
import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def zero = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    def zero = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2
    def zero = (a: A) => a
  }

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    def op(x: A, y: A) = m.op(y, x)
    def zero = m.zero
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(for {
      x <- gen
      y <- gen
      z <- gen
    } yield (x, y, z)) {
      case (x, y, z) =>
        m.op(x, m.op(y, z)) == m.op(m.op(x, y), z)
    } &&
      forAll(gen)((a: A) => m.op(m.zero, a) == a && m.op(a, m.zero) == a)

  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    ???

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, a) => m.op(acc, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap(as, endoMonoid[B])(f.curried)(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.size == 0) m.zero
    else if (as.size == 1) f(as.head)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean = {
    val ordMon = new Monoid[Option[(Int, Int, Boolean)]] {
      def op(a: Option[(Int, Int, Boolean)], b: Option[(Int, Int, Boolean)]) =
        (a, b) match {
          case (Some((x1, y1, p)), Some((x2, y2, q))) =>
            Some(x1 min x2, y1 max y2, p && q && y1 < y2)
          case (x, None) => x
          case (None, x) => x
        }
      def zero = None
    }
    foldMapV(ints, ordMon)(a => Some((a, a, true))).map(_._3).getOrElse(true)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    def op(p1: Par[A], p2: Par[A]): Par[A] = p1.map2(p2)(m.op)
    def zero = Par.unit(m.zero)
  }

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par.parMap(v)(f).flatMap(isb => foldMapV(isb, par(m))(a => Par.lazyUnit(a)))
    

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    def op(p1: WC, p2: WC): WC =
      (p1, p2) match {
        case (Stub(a), Stub(b))                   => Stub(a + b)
        case (Stub(a), Part(l, w, r))             => Part(a + l, w, r)
        case (Part(l, w, r), Stub(a))             => Part(l, w, r + a)
        case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
          //                in case of a whitespace
          //                _____^_____________________
          Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
      }
    def zero = Stub("")
  }

  def count(s: String): Int = {
    foldMapV(s.toIndexedSeq, wcMonoid)(
      c => if (c.isWhitespace) Part("", 0, "") else Stub(c.toString)
    ) match {
      case Stub(s) => s.length min 1
      case Part(l, w, r) =>
        (l.length min 1) + w + (l.length min 1) // l or r can be empty, which means 0 or max one word
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a1._1), B.op(a1._2, a2._2))
    def zero = (A.zero, B.zero)
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    def op(a1: A => B, a2: A => B): A => B =  a => B.op(a1(a), a2(a))
    def zero: A => B = a => B.zero
  }

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K,V]] {
    def op(a1: Map[K,V], a2: Map[K,V]): Map[K,V] = 
      (a1.keySet ++ a2.keySet).foldLeft(zero){(acc, k) =>
        acc.updated(k, V.op(a1.getOrElse(k, V.zero), a2.getOrElse(k, V.zero)))
      }
    def zero: Map[K,V] = Map[K, V]()
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV(as, mapMergeMonoid[A, Int](intAddition))(a => Map(a -> 1))
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(endoMonoid[B])(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = 
    foldRight(as)(mb.zero)((a, acc) => mb.op(f(a), acc))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    ???

  def toList[A](as: F[A]): List[A] =
   foldRight(as)(List.empty[A])(_ :: _)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldLeft(mb.zero)((acc, a) => mb.op(acc, f(a)))

}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  import Monoid._
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  import Monoid._
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb)) 
  }
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    foldMap(as)(a => (b: B) => f(b, a))(dual(endoMonoid[B]))(z)
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    foldMap(as)(f.curried)(endoMonoid[B])(z)
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case None => mb.zero
    case Some(a) => f(a)
  }
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) = 
    as.foldLeft(z)(f)
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
}
