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
     case Nil => sys.error("empty")
     case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
     case Nil => sys.error("empty")
     case Cons(_,t) => Cons(h,t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
	if (n == 0) l
	else l match {
	     case Nil => sys.error("not enough elements")
	     case Cons(_,t) => drop(t, n-1)
        }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
	case Nil => Nil
        case Cons(h,t) => if (f(h)) dropWhile(t,f) else l
  }

  def init[A](l: List[A]): List[A] = {
	def init_recur[A](l:List[A], acc:List[A]): List[A] =
		l match {
			case Nil => sys.error("this should not happen")
			case Cons(x, Nil) => acc
			case Cons(x, t) => init_recur(t, Cons(x,acc))
		}

	def reverse_recur[A](l:List[A], acc:List[A]): List[A] = l match {
		case Nil => acc
		case Cons(h,t) => reverse_recur(t,Cons(h,acc))
	}
	reverse_recur(init_recur(l, Nil), Nil)
		
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,acc) => 1 + acc)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
	@annotation.tailrec
	def recur[A,B](l:List[A], f: (B,A) => B, acc:B) : B =
		l match {
			case Nil => acc
			case Cons(h,t) => recur(t, f, f(acc, h))
                } 
	recur(l, f, z)	
  }

  def leftSum(l: List[Int]):Int = foldLeft(l, 0)((acc, x) => acc + x)
  def leftProduct(l:List[Double]):Double = foldLeft(l, 1.0)((acc, x) => acc * x)
  def leftLength[A](l:List[A]):Int = foldLeft(l, 0)((acc,_) => 1 + acc)
  def reverse[A](l:List[A]):List[A] = foldLeft(l, Nil:List[A])((acc:List[A],x) => Cons(x,acc))
  def ident[A](l:List[A]):List[A] = foldRight(l, Nil:List[A])((x,acc) => Cons(x,acc))
  def rightAppend[A](a1:List[A], a2:List[A]):List[A] = foldRight(a1, a2)((x,acc) => Cons(x,acc))
  def leftAppend[A](a1:List[A], a2:List[A]):List[A] = foldLeft(reverse(a1), a2)((acc,x) => Cons(x,acc))
  def listCat[A](l:List[List[A]]): List[A] = foldLeft(reverse(l),Nil:List[A])((acc,x) => leftAppend(x,acc))

  def addOneToEach(l:List[Int]):List[Int] = foldLeft(reverse(l),Nil:List[Int])((acc,x) => Cons(x+1,acc))
  def stringificate(l:List[Double]):List[String] = foldLeft(reverse(l),Nil:List[String])((acc,x) => Cons(x.toString(), acc))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldLeft(reverse(l),Nil:List[B])((acc,x) => Cons(f(x), acc))
  def filter[A,B](l: List[A])(f: A => Boolean): List[A] = foldLeft(reverse(l),Nil:List[A])((acc,x) => if (f(x)) Cons(x, acc) else acc)
  def flatMap[A,B](as:List[A])(f: A=> List[B]) : List[B] = listCat(map(as)(f))
}
