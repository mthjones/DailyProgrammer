package dp180e

object LookNSayPar {
  import dp180e.Util._

  def looknsay(n: Int, seed: String): String = times(n-1, seed)(describe)

  def seqop(l: List[(Char, Int)], c: Char): List[(Char, Int)] = l match {
    case h :: t if h._1 == c => (c, h._2 + 1) :: l.tail
    case _ => (c, 1) :: l
  }

  def combine(l: List[(Char, Int)], r: List[(Char, Int)]): List[(Char, Int)] = {
    if (l.isEmpty) r
    else {
      val last = l.last
      if (last._1 == r.head._1) l.init ::: List((last._1, last._2 + r.head._2)) ::: r.tail
      else l ::: r
    }
  }

  def describe(s: String): String =
    s.toStream.par.aggregate(List[(Char, Int)]())(seqop, combine).map { case (c, i) => i.toString + c }.mkString

  def main(args: Array[String]) = println(args.headOption.flatMap(_.toIntOption) match {
    case Some(i) => looknsay(i, args.tail.headOption.getOrElse("1"))
    case None => "Usage: scala dp180e.LookNSay n [seed]"
  })
}
