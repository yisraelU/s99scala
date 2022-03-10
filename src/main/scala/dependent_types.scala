import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object dependent_types extends App {

  trait Foo {
    type T
    def t: T
  }
  object StringFoo extends Foo {
    type T = String
    override def t = "hi"
  }
  object IntFoo extends Foo {
    override type T = Int

    override def t = 998
  }

  def getValue(f: Foo): f.T = {
    f.t
  }

  trait DepValue {
    type V
    val value: V

  }
  def magic(depValue: DepValue): depValue.V = {
    depValue.value
  }
  def mk[T](t: T): DepValue {
    type V = T
  } = {
    new DepValue {
      override type V = T
      override val value = t
    }
  }
  val strDep: DepValue {
    type V = String
  } = mk("hi")
  val intDep: DepValue {
    type V = Int
  } = mk(998)
  val str: String = magic(strDep)
  val int: Int = magic(intDep)
}

trait Foo {
  class Bar
  def doNothing(b: Bar) {}
}
object Foo {
  val f1 = new Foo {}
  val b1: f1.Bar = new f1.Bar()
  val f2 = new Foo {}
  val b2: f2.Bar = new f2.Bar()
}
trait Inner[F] {
  type T
}

object Inner {
  def apply[F](implicit inner: Inner[F]) = inner

  implicit def mk[F, A] = new Inner[F] {
    type T = A
  }
}
trait IsFuture[F] {
  type T

  def apply(f: F): Future[T]
}

object IsFuture {
  def apply[F](implicit isf: IsFuture[F]) = isf

  implicit def mk[A] = new IsFuture[Future[A]] {
    type T = A

    def apply(f: Future[A]): Future[A] = f
  }

  def logResult[A](thing: A)(implicit isf: IsFuture[A]): Future[isf.T] =
    isf(thing) map { x =>
      println(s"I got a result of $x")
      x
    }

}
