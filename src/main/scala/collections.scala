import java.util.NoSuchElementException
import scala.annotation.tailrec

object collections {
  sealed trait List[+A] {
    self =>
    def ::[A1 >: A](a: A1): List[A1] = new ::(a, self)
    def prepend[A1 >: A](a: A1): List[A1] = a :: self
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
    def penultimate: A = self.foldLeft[(A, Int)]()
    def length: Int = self.foldLeft(0)((l, _) => l + 1)
    def tail: List[A] = self.foldLeft(List.empty[A]) { (acc, _) =>
      self match {
        case Nil => Nil
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
  }
  case object Nil extends List[Nothing]
  case class ::[A](a: A, list: List[A]) extends List[A]

  object List {
    def apply[A](a: A*): List[A] =
      if (a.isEmpty) Nil
      else a.head :: apply(a.tail: _*)
    def fill[A](n: Int)(elem: => A): List[A] = {
      def loop(n: Int): List[A] = {
        if (n <= 0) Nil
        else elem :: loop(n - 1)
      }
      loop(n)
    }
    def empty[A]: List[A] = Nil
  }

}
object tests extends App {
  println(collections.List(1, 5, 5, 6, 3, 4, 5, 6, 7, 8, 9).kth(3))
}
