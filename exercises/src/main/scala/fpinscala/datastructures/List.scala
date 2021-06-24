package fpinscala.datastructures

import scala.annotation.tailrec

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

  val x:Int = List(1,2,3,4,5) match {
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

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  /* Exercise 3.7
   * Can product, implemented using foldRight, immediately halt the recursion and return 0.0
   * if it encounters a 0.0?  Why or why not?
   *
   * No, foldRight would need to know what function it is being applied to in order to know that it could short
   * circuit the logic.
   */
  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  // Exercise 3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
  }

  // Exercise 3.6
  // This can't be constant time like tail because it must walk through the entire list to reach what it must drop.
  def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc) => acc+1)

  // Exercise 3.10
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(remaining: List[A], acc: B): B = remaining match {
      case Nil => acc
      case Cons(x, xs) => go(xs, f(acc, x))
    }

    go(l, z)
  }

  // Exercise 3.11
  // Still need to get used to more generic anonymous functions...
  // ((acc, a) => acc + a) ==> (_ + _)
  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc+1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List())((acc, a) => Cons(a, acc))

  // Exercise 3.13
  // Can you write foldLeft in terms of foldRight?  How about the other way around?
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    // Using (b:B) => b allows the evaluation of the function to be delayed...
    foldRight(l, (b:B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a2, a1)(Cons(_, _))

  // Exercise 3.15
  def concatLists[A](l: List[List[A]]): List[A] =
    foldRight(l, List())((a, b) => foldRight(a, b)(Cons(_, _)))
  // foldRight(l, Nil:List[A])(append)
  //  - I missed that append is basically the same thing as what I wrote.
  //  - What's the difference between List() and Nil:List[A]?

  // Exercise 3.16
  def add1_original(l: List[Int]): List[Int] = {
    @tailrec
    def go(l: List[Int], acc: List[Int]): List[Int] = l match {
      case Nil => acc
      case Cons(h, t) => go(t, Cons(h+1, acc))
    }

    go(l, List())
  }

  // Provided in the answer key...I'm still working on reusing when building something up...
  def add1(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((a, b) => Cons(a + 1, b))
  }

  // Exercise 3.17
  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))

  // Exercise 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))
}
