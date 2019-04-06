package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def unit[A](a: A): Tree[A] = Leaf(a)

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximun(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximun(left) max maximun(right)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l ,r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size1[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _)
  def maximun1(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)
  def depth1[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((a, b) => 1 + a max b)
  def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}




