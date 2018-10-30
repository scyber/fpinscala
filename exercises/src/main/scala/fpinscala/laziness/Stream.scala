package fpinscala.laziness

import fpinscala.laziness.Stream.{cons, empty, unfold}

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  def toList: List[A] = foldRight(List(): List[A])(_ :: _)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(p(a)) cons(a, b) else empty)

  def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](p: A => B): Stream[B] = foldRight(empty[B])((a,b) => cons(p(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a,b) => if(p(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((a,b) => cons(a, b))

  def flatMap[B](p: A => Stream[B]): Stream[B] = foldRight(empty[B])((a,b) => p(a) append b)

  def map1[B](p: A => B): Stream[B] = unfold(this) {
    case Cons(h, tail) => Some((p(h()), tail()))
    case Empty => None
  }

  def take1(n: Int): Stream[A] = unfold((n,this)) {
    case (curr, Cons(h, tail)) if curr > 0 => Some(h(), (curr - 1, tail()))
    case _ => None
  }

  def takeWhile1(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, tail) if p(h()) => Some(h(), tail())
    case _ => None
  }

  def zipWith[B,C](z: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, z)) {
    case (Cons(h, tail), Cons(h1, tail1)) => Some(f(h(), h1()), (tail(), tail1()))
    case _ => None
  }

  def zipAll[B](z: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, z)) {
    case (Empty,Empty) => None
    case (Cons(h, tail), Empty) => Some((Some(h()), Option.empty) -> (tail(), empty))
    case (Empty, Cons(h1, tail1)) => Some((Option.empty, Some(h1())) -> (empty, tail1()))
    case (Cons(h, tail), Cons(h1, tail1)) => Some((Some(h()), Some(h1())) -> (tail(), tail1()))
  }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile { case (_,b) => b.isDefined }.forAll { case (a,b) => a == b }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, tail) => Some((cons(h(), tail()), tail()))
    case _ => None
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

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

  val ones: Stream[Int] = cons(1, ones)

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibs(curr: Int, prev: Int): Stream[Int] = cons(curr, fibs(curr + prev, curr))
    fibs(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }
  }

  def constant1[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))

  def from1(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))

  def fibs1: Stream[Int] = unfold((0,1)){ case (n1, n2) => Some((n1, (n2, n1 + n2)))}

}