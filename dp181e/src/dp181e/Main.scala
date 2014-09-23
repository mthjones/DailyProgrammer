package dp181e

import dp181e.Equations.Equation

object Main {
  def main(args: Array[String]) {
    println("Equation solver! Enter equations on separate lines. Enter q to quit.")
    for (line <- io.Source.stdin.getLines().takeWhile(_ != "q")) {
      Equation.parse(line) match {
        case Some(e1) => io.Source.stdin.getLines().map(Equation.parse).next() match {
          case Some(e2) => e1.intersect(e2) match {
            case Some(i) => println(i)
            case None => println("Parallel lines, no intersection.")
          }
          case None => println("Invalid equation.")
        }
        case None => println("Invalid equation.")
      }
    }
  }
}
