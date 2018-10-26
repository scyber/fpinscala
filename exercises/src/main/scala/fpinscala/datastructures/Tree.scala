package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def max(tree: Tree[Int]): Int = fold(tree)(a => a)(_ max _)

  def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((a,b) => 1 + (a max b))

  def map[A](tree: Tree[A])(f: A => A): Tree[A] = fold(tree)(a => Leaf(f(a)): Tree[A])(Branch(_,_))

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }


}