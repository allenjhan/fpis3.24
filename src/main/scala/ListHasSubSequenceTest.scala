object ListHasSubSequenceTest extends App{
  val myList1 = List(0,1,2,3,4,5)
  val myList2 = List(0,1)
  val myList3 = List(3,4)
  val myList4 = List(3,4,5)
  val myList5 = List(4,5)
  val myList6 = List(1,5)
  val myList7 = List(1,0)
  val myList8 = List(2,4)
  val myList9 = List(3,2)
  val bigList = List(myList2, myList3, myList4, myList5, myList6, myList7, myList8, myList9)
  println(List.map(bigList)(x => List.hasSubSequence(myList1, x)))

}

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


  def tail[A](l: List[A]): List[A] = ???

  def setHead[A](l: List[A], h: A): List[A] = ???

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(x, xs) => if (n == 0) l else drop(xs, n -1)
    case Nil => Nil
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = ???

  def init[A](l: List[A]): List[A] = ???

  def length[A](l: List[A]): Int = {
    def inner(m: List[A], n: Int): Int = m match {
      case Cons(x, xs) => inner(xs, n+1)
      case Nil => n
    }
    inner(l, 0)
  }

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((e, acc)=>Cons(f(e), acc))

  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    def inner(l1: List[A], l2:List[A]): Boolean = {
      l2 match {
        case Cons(x, xs) =>
          l1 match {
            case Cons(y, ys) => if (x == y) inner(ys, xs) else false
            case Nil => false
          }
        case Nil => true
      }
    }

    def createListOfLists[A](l: List[A]): List[List[A]] = {
      val res = foldRight(l, (0, Nil):Tuple2[Int, List[List[A]]]){(e, acc) =>
        (acc._1 + 1, Cons(drop(l, acc._1), acc._2))
      }
      res._2
    }

    val toFold = map(createListOfLists(sup))(x => inner(x, sub))
    foldRight(toFold, false)((e, acc)=>e||acc)
  }
}