import rockthejvm_interviewprep.lists.{RList, RNil}

import java.util.NoSuchElementException
import scala.annotation.tailrec

object rockthejvm_interviewprep extends App {

  object lists {
    // naturally recursive
    sealed abstract class RList[+T] { self =>
      def head: T
      def headOption: Option[T]
      def tail: RList[T]
      def isEmpty: Boolean
      def ::[U >: T](elem: U): RList[U] = new ::(elem, self)
      def toString: String
      def apply(index: Int): T
      def length: Int
      def reverse: RList[T]
    }
    object RList {
      def from[T](iterable: Iterable[T]): RList[T] = {
        @tailrec
        def loop(iterable: Iterable[T], acc: RList[T]): RList[T] = {
          if (iterable.isEmpty) acc
          else
            loop(iterable.tail, iterable.head :: acc)
        }
        loop(iterable, RNil)
      }
    }
    case object RNil extends RList[Nothing] { self =>
      override def head = throw new NoSuchElementException("head of empty list")
      override def tail = throw new NoSuchElementException("tail of empty list")
      override def isEmpty = true
      override def headOption = None
      override def toString = "[]"
      override def apply(index: Int) = head

      override def length = 0

      override def reverse = self
    }
    case class ::[T](override val head: T, override val tail: RList[T])
        extends RList[T] { self =>
      override def headOption = Some(head)
      override def isEmpty = false
      override def toString = self match {
        case ::(h, t) => s"::($h,${t.toString})"
      }

      override def apply(index: Int) = {
        @tailrec
        def loop(i: Int, list: RList[T]): T = {
          (i, list) match {
            case (i, list) =>
              list match {
                case RNil =>
                  throw new NoSuchElementException("head of empty list")
                case ::(head, tail) => if (i == 0) head else loop(i - 1, tail)
              }
          }
        }
        loop(index, self)
      }

      override def length = {
        @tailrec
        def loop(list: RList[T], length: Int): Int = {
          list match {
            case RNil           => length
            case ::(head, tail) => loop(tail, 1 + length)
          }
        }
        loop(self, 0)
      }

      override def reverse = {
        @tailrec
        def loop(list: RList[T], acc: RList[T]): RList[T] = {
          if (list.isEmpty) acc
          else
            loop(list.tail, list.head :: acc)
        }
        loop(self, RNil)
      }
    }

  }
  val list = 1 :: 2 :: 3 :: RNil
  RList.from((1 to 100))
  println(list.reverse)

}
