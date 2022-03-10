import scala.annotation.tailrec

object advanced_scala {

  object partial_functions {
    val pf1: PartialFunction[Int, Int] = {
      case 1 => 5
    }
    val pf2: PartialFunction[Int, Int] = Function.unlift(pf1.lift)
    val pf3: PartialFunction[Int, Int] = pf1.orElse(pf2)
  }

  object functional_set {
    sealed trait FSet[A] extends (A => Boolean) {
      def contains(a: A): Boolean
      override def apply(v1: A): Boolean = contains(v1)
      def +(a: A): FSet[A]
      def ++(fset: FSet[A]): FSet[A]
      def map[B](f: A => B): FSet[B]
      def flatMap[B](f: A => FSet[B]): FSet[B]
      def filter(predicate: A => Boolean): FSet[A]
      def forEach(f: A => Unit): Unit
    }
    case class Empty[A]() extends FSet[A] { self =>
      override def contains(a: A) = false

      override def +(a: A) = Cons(a, self)

      override def ++(fset: FSet[A]) = fset

      override def map[B](f: A => B) = Empty[B]()

      override def flatMap[B](f: A => FSet[B]) = Empty[B]()

      override def filter(predicate: A => Boolean) = self

      override def forEach(f: A => Unit) = ()
    }
    case class Cons[A](head: A, tail: FSet[A]) extends FSet[A] { self =>
      override def contains(a: A) = head == a || tail.contains(a)

      override def +(a: A) = Cons(a, self)

      override def ++(fset: FSet[A]) = {
        @tailrec
        def loop(fset: FSet[A], acc: FSet[A]): FSet[A] = fset match {
          case Empty()          => acc
          case Cons(head, tail) => loop(tail, acc + head)
        }
        loop(fset, self)
      }

      override def map[B](f: A => B) = {
        def loop(fset: FSet[A], acc: FSet[B]): FSet[B] = {
          fset match {
            case Empty()          => acc
            case Cons(head, tail) => loop(tail, acc + f(head))
          }
        }
        loop(self, Empty[B]())
      }

      override def flatMap[B](f: A => FSet[B]) = {
        f(head) match {
          case Empty() => tail.flatMap(f)
          case lset    => lset ++ tail.flatMap(f)
        }
      }

      override def filter(predicate: A => Boolean) = {
        self match {
          case Cons(head, tail) =>
            if (predicate(head)) Cons(head, tail.filter(predicate))
            else tail.filter(predicate)
        }
      }

      override def forEach(f: A => Unit) = {
        f(head)
        tail.forEach(f)
      }

    }
    object FSet {

      def apply[A](a: A*): FSet[A] = {
        @tailrec
        def loop(values: Seq[A], acc: FSet[A]): FSet[A] = {
          if (values.isEmpty) acc
          else loop(values.tail, acc + a.head)
        }
        loop(a, Empty())
      }
    }
  }
  object inheritance extends App {
    trait Cold {
      def print: Unit = println("cold")
    }
    trait Green extends Cold {
      override def print = println("green")
      super.print
    }
    trait Blue extends Cold {
      override def print = println("blue")
      super.print
    }
    class Red {
      def print = println("red")
    }
    class White extends Red with Blue with Green {
      override def print = println("white"); super.print
    }
    val white = new White
    white.print
  }
}

object inheritance extends App {
  trait Cold {
    def print: Unit = println("cold")
  }
  trait Green extends Cold {
    override def print = println("green")
    super.print
  }
  trait Blue extends Cold {
    override def print = println("blue")
    super.print
  }
  class Red {
    def print = println("red")
  }
  class White extends Red with Blue with Green {
    override def print = println("white-----------");
    Thread.sleep(10000)
    super.print
  }
  val white = new White
  white.print
}
object variance {
  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  abstract class Parking[T](var list: List[T]) {
    def park(vehicle: T): Unit = {
      list = vehicle :: list
    }
    def impound(vehicles: List[T]): Unit = {
      list = list.diff(vehicles)
    }
    def checkvehicles(condition: String): List[T] = {
      list.filter(_.getClass.getName.contains(condition))
    }
  }
}
