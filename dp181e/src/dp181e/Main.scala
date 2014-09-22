package dp181e

import dp181e.Equations.Equation

object Main {
  def main(args: Array[String]) {
    println("Equation solver! Enter equations on separate lines. Enter q to quit.")
    var prevEq: Option[Equation] = None
    for (line <- io.Source.stdin.getLines().takeWhile(_ != "q")) {
      Equation.parse(line) match {
        case Some(e) => prevEq match {
          case Some(e2) =>
            e.intersect(e2) match {
              case Some(i) => println(i)
              case None => println("Parallel lines, no intersection")
            }
            prevEq = None
          case None => prevEq = Some(e)
        }
        case None => println("Invalid equation.")
      }
    }
  }
}
