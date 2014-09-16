package roguelike

import roguelike.Rogue.Direction

case class Map(width: Int, height: Int, coins: Set[Position]) {
  def display: String = List.fill(height*width)(Map.openSymbol).zipWithIndex
    .map { case (c, i) => characterForPosition(Position.fromIndex(width)(i)) }
    .grouped(width)
    .map(_.mkString).mkString("\n")

  def characterForPosition(p: Position): Char = {
    if (isWall(p)) Map.wallSymbol
    else if (coins.contains(p)) Map.coinSymbol
    else Map.openSymbol
  }

  def isValidMove(p: Position, dir: Direction): Boolean = !isWall(p.move(dir))

  def isWall(p: Position): Boolean = p match {
    case Position(0, _) => true
    case Position(x, _) if x == width - 1 => true
    case Position(_, 0) => true
    case Position(_, y) if y == height - 1 => true
    case _ => false
  }
}

object Map {
  val playerSymbol: Char = '@'
  val coinSymbol: Char = '$'
  val wallSymbol: Char = '%'
  val openSymbol: Char = '.'
}
