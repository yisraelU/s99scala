import java.util.NoSuchElementException
import scala.annotation.tailrec

object collections {
  trait Equals[-B] {
    def ==(b: B, b2: B): Boolean
  }
  sealed trait List[+A] {
    self =>
    def ::[A1 >: A](a: A1): List[A1] = new ::(a, self)
    def prepend[A1 >: A](a: A1): List[A1] = a :: self
    def append[A1 >: A](a: A1): List[A1] =
      self.foldRight[List[A1]](new ::(a, Nil)) { (a, acc) =>
        a :: acc
      }
    def foldLeft[B](acc: B)(f: (B, A) => B): B = {
      @tailrec
      def loop(list: List[A])(acc: B): B = {
        list match {
          case Nil         => acc
          case ::(a, list) => loop(list)(f(acc, a))
        }
      }
      loop(self)(acc)
    }
    def foldRight[B](acc: B)(f: (A, B) => B): B = {
      def loop(list: List[A])(acc: B): B = {
        list match {
          case Nil         => acc
          case ::(a, list) => f(a, loop(list)(acc))
        }
      }
      loop(self)(acc)
    }
    def fold[A1 >: A](acc: A1)(f: (A1, A) => A1): A1 = foldLeft(acc)(f)
    def append[A1 >: A](that: List[A1]): List[A1] = {
      self
        .foldLeft(that) { (acc, a) =>
          a :: acc
        }
    }
    def collect[B](pf: PartialFunction[A, B]): List[B] = {
      self
        .foldLeft[List[B]](Nil) { (acc, a) =>
          if (pf.isDefinedAt(a)) pf(a) :: acc
          else acc
        }
        .reverse
    }
    def mapSome[A1 >: A](pf: PartialFunction[A, A1]): List[A1] = {
      self
        .foldLeft[List[A1]](Nil) { (acc, a) =>
          if (pf.isDefinedAt(a)) pf(a) :: acc
          else a :: acc
        }
        .reverse
    }
    def reverse: List[A] = {
      self.foldLeft[List[A]](Nil)((acc, a) => a :: acc)
    }
    def head: A = self match {
      case Nil      => throw new NoSuchElementException
      case ::(a, _) => a
    }
    def last: A = self.foldLeft[A](self.head) { (_, a) =>
      a
    }
    def kth(i: Int): A =
      self
        .foldLeft[(A, Int)]((self.head, i)) {
          case ((acc, 0), _) => (acc, 0)
          case ((_, i), a)   => (a, i - 1)
        }
        ._1
    def penultimate: A =
      self
        .foldLeft[(A, A)]((self.head, self.tail.head)) { (acc, a) =>
          (acc._2, a)
        }
        ._1
    def forAll(p: A => Boolean): Boolean =
      self.foldLeft(true) {
        case (b, a) => b && p(a)
      }
    def exists(p: A => Boolean): Boolean = self.foldLeft(false) { (b, a) =>
      b || p(a)
    }
    def length: Int = self.foldLeft(0)((l, _) => l + 1)
    def tail: List[A] = self.foldLeft(List.empty[A]) { (acc, _) =>
      self match {
        case Nil => acc
        case ::(a, list) =>
          acc match {
            case Nil => list
            case t   => t
          }
      }
    }
    def setHead[A1 >: A](a: A1): List[A1] =
      self.foldLeft(a :: Nil)((list, a) => {
        a :: list
      })
    def drop(n: Int): List[A] = {
      self
        .foldLeft[(List[A], Int)]((Nil, n)) {
          case ((acc, 0), a) => (a :: acc, 0)
          case ((acc, i), a) => (acc, i - 1)
        }
        ._1
        .reverse
    }
    def dropWhile(f: A => Boolean): List[A] = {
      self
        .foldLeft[(List[A], Boolean)]((Nil, true)) {
          case ((acc, true), a) =>
            if (f(a)) (acc, true) else (a :: acc, false)
          case ((acc, false), a) => (a :: acc, false)
        }
        ._1
        .reverse
    }
    def forEach(f: A => Unit): Unit = self.foldLeft(()) { (_, a) =>
      f(a)
    }
    def partition(f: A => Boolean): (List[A], List[A]) = {
      self.foldRight((List.empty[A], List.empty[A])) { (a, acc) =>
        {
          if (f(a)) (a :: acc._1, acc._2)
          else (acc._1, a :: acc._2)
        }
      }
    }
    def find(p: A => Boolean): Option[A] = {
      self.foldLeft[Option[A]](None) { (acc, a) =>
        if (acc.isEmpty && p(a)) Some(a) else acc
      }

    }
    def plainZip[A1 >: A](that: List[A1]): List[(A1, A1)] =
      (self, that) match {
        case (Nil, _) | (_, Nil)        => List.empty[(A1, A1)]
        case (::(h1, tl1), ::(h2, tl2)) => (h1, h2) :: tl1.plainZip(tl2)
      }

    def zip[A1 >: A](that: List[A1]): List[(A1, A1)] =
      self
        .foldLeft((List.empty[(A1, A1)], that)) { (acc, a) =>
          {
            acc._2 match {
              case Nil       => acc
              case ::(b, tl) => ((a, b) :: acc._1, tl)
            }
          }
        }
        ._1
        .reverse

    def isPalindrome: Boolean = self.reverse == self
    // requires equivalence type class
    def compress(implicit ev: Equals[A]): List[A] =
      self
        .foldRight((List.empty[A], self.head)) { (a, acc) =>
          if (a == acc._2) acc
          else (a :: acc._1, a)
        }
        ._1
    /*   def pack(implicit ev: Equals[A]): List[List[A]] = self.foldRight((List(List.empty[A]),self.head)){
      case (a,(acc,h)) => if(a == h) (::(a,Nil) :: acc , a) else
    }._1*/

    def duplicate: List[A] = self.foldRight(List.empty[A]) { (a, acc) =>
      a :: a :: acc
    }
    def duplicateN(n: Int): List[A] = self.foldRight(List.empty[A]) {
      (a, acc) =>
        List.fill(n)(a).append(acc)
    }
  }
  case object Nil extends List[Nothing]
  case class ::[A](a: A, list: List[A]) extends List[A]

  object List {
    def apply[A](a: A*): List[A] =
      if (a.isEmpty) Nil
      else a.head :: apply(a.tail: _*)

    def unapplySeq() = ???

    def fill[A](n: Int)(elem: => A): List[A] = {
      def loop(n: Int): List[A] = {
        if (n <= 0) Nil
        else elem :: loop(n - 1)
      }
      loop(n)
    }
    def empty[A]: List[A] = Nil
    def flatten[A](list: List[List[A]]): List[A] =
      list.foldRight(List.empty[A]) { (a, acc) =>
        a match {
          case Nil => acc
          case ::(a, list) =>
            a :: list.foldRight(acc) { (a, acc) =>
              a :: acc
            }
        }
      }
  }

}
object tests extends App {
  implicit val iEquals = new collections.Equals[Int] {
    override def ==(b: Int, b2: Int) = b == b2
  }
  val pal = collections.List(1, 2, 3, 4, 5, 6, 6)
  println(pal.duplicateN(7))
  println(pal.compress)
  println(pal.append(7).append(5).append(900))

  val list = collections.List(1, 5, 5, 3, 4, 5, 7, 8, 10, 9)
  val flat = collections.List.flatten(collections.List(pal, list))
  println(pal.zip(list))
  println(flat)
  println(list.forAll(_.>(1)))
}
