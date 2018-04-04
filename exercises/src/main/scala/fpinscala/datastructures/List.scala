package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/*
 * Exercise 3.1: What will be the result of the following match expression?
 *
 * val x = List(1,2,3,4,5) match {
 *   case Cons(x, Cons(2, Cons(4, _))) => x
 *   case Nil => 42
 *   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
 *   case Cons(h, t) => h + sum(t)
 *   case _ => 101
 * }
 *
 * Answer: 3
 *
 * Though I tried running in the REPL and it kept complaining about the Cons and Nil objects.
 */

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

  /* Exercise 3.8
   * See what happens when you pass Nil and Cons themselves to foldRight.  What do you think this
   * says about the relationship between foldRight and the data constructors of List?
   *
   * I'm not sure I'm understanding the question correctly, but to me it seems they are not
   * coupled.  Still not sure even after looking at the answer, but their assertion that we get
   * back the same list is what I expected - guess I just didn't see that as a big deal.
   */
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  /* Exercise 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and return 0.0
   * if it encounters a 0.0?  Why or why not?
   *
   * foldRight would need to know what function it is being applied to know that it should short
   * circuit the logic.
   */
  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, ls) => drop(ls, n-1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((a,b) => b+1)

  // Exercise 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def go(remaining: List[A], acc: B): B = remaining match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }

    go(l, z)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
