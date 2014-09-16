package dp180e

object LookNSay {
  import dp180e.Util._

  def looknsay(n: Int, seed: String): String = times(n-1, seed)(describe)
  def describe(s: String): String =
    s.foldWithTake("") { s1 => _ == s1.head } { (s1, acc) => acc + s1.length + s1.head }

  def main(args: Array[String]) = println(args.headOption.flatMap(_.toIntOption) match {
    case Some(i) => looknsay(i, args.tail.headOption.getOrElse("1"))
    case None => "Usage: scala dp180e.LookNSay n [seed]"
  })
}
