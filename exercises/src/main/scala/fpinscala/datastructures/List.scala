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
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(_,_) if n == 0 => l
    case Cons(_, tail) => drop(tail, n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,tail) if f(h) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_,Nil) => Nil
      case Cons(h,tail) => Cons(h, init(tail))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,len) => len + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int = foldRight(l, 0)((_,len) => len + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc,elem) => Cons(elem, acc))

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b,a) => f(a,b))

  def appendFold[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((b,a) => Cons(a,b))

  def concat[A](as: List[A]*): List[A] =
    if (as.isEmpty) Nil
    else appendFold(as.head, concat(as.tail: _*))

  def addOne(ints: List[Int]): List[Int] = foldRight2(ints, Nil: List[Int])((elem, acc) => Cons(elem + 1, acc))

  def doubleToString(doubles: List[Double]): List[String] =
    foldRight2(doubles, Nil: List[String])((elem, acc) => Cons(elem.toString, acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight2(l, Nil: List[B])((a,b) => Cons(f(a), b))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight2(as, Nil: List[A])((a, b) => if(f(a)) Cons(a, b) else b)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight2(as, Nil: List[B])((a,b) => appendFold(f(a), b))

  def filterFM[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a: A) => if(f(a)) List(a) else Nil)

  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = (l1, l2) match {
    case (Cons(h1, tail1), Cons(h2, tail2)) => Cons(f(h1, h2), zipWith(tail1, tail2)(f))
    case _ => Nil
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def contains(l: List[A], elem: A) = filter(l)(_ == elem) match {
      case Nil => false
      case _ => true
    }
    foldRight2(sub, true)((el, ok) => ok && contains(sup, el))
  }


}
