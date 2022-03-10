// handy functions for common leetcode sub routines
object leetcode_utils extends App {

  // reverse a number
  // can be used in problems that store a number in a list or array
  def reverse(x: Int): Int = {
    val i = Math.abs(x)
    val list = List.unfold(i)(s => Option.when(s > 0)((s % 10, s / 10)))
    val res = list.reverse.foldLeft(0)((acc, elem) => acc * 10 + elem)
    if (x < 0) -res
    else res
  }

  // powerset function derive powerset of a list
  def powerset[A](list: List[A]): List[List[A]] = {
    def loop(list: List[A], acc: List[List[A]]): List[List[A]] = {
      list match {
        case Nil     => acc
        case x :: xs => loop(xs, acc) ++ acc.map(x :: _)
      }
    }
    loop(list, List(Nil))
  }

}
