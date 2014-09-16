package dp180e

import scala.annotation.tailrec

object Util {
  implicit class DP180StringOps(s: String) {
    def foldWithTake[B](z: B)(f: String => Char => Boolean)(g: (String, B) => B): B = {
      @tailrec
      def loop(s1: String, acc: B): B =
        if (s1.isEmpty) acc
        else {
          val (eq, tail) = s1.span(f(s1))
          loop(tail, g(eq, acc))
        }
      loop(s, z)
    }

    def toIntOption: Option[Int] = try {
      Some(s.toInt)
    } catch {
      case _: NumberFormatException => None
    }
  }

  def times[B](n: Int, z: B)(f: B => B): B = List.fill(n)(f).foldRight(z)(_(_))
}
