package roguelike

import roguelike.Rogue.{Direction, North, South, East, West}

case class Position(x: Int, y: Int) {
  def move(dir: Direction): Position = dir match {
    case North => copy(y = y - 1)
    case South => copy(y = y + 1)
    case East => copy(x = x + 1)
    case West => copy(x = x - 1)
  }

  def toIndex(w: Int): Int = y * w + x
}

object Position {
  def fromIndex(w: Int)(i: Int): Position = Position(i % w, i / w)
}