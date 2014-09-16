package roguelike

import scalaz._
import scalaz.effect.IO

object Rogue extends App {
  type Game[A] = StateT[IO, GameState, A]

  def movePlayer(dir: Direction): Game[(Int, Int, Map, PlayerState)] = state { (s: GameState) =>
    def updatedPlayerAndMap(p: PlayerState, m: Map): (PlayerState, Map) = {
      val newPosition = p.position.move(dir)
      if (m.coins.contains(newPosition)) {
        (p.copy(position = newPosition, score = p.score + 1), m.copy(coins = m.coins - newPosition))
      } else {
        (p.copy(position = newPosition), m)
      }
    }

    if (s.map.isValidMove(s.player.position, dir)) {
      val (newPlayer, newMap) = updatedPlayerAndMap(s.player, s.map)
      (s.copy(map = newMap, player = newPlayer, moves = s.moves - 1), (s.moves - 1, newPlayer.score, newMap, newPlayer))
    } else (s, (s.moves, s.player.score, s.map, s.player))
  }

  def parse(s: String): GameState = {
    val width = s.split("\n").head.size
    val height = s.split("\n").size
    val playerPos = Position.fromIndex(width)(s.filter(_ != '\n').indexOf(Map.playerSymbol))
    val coins = s.filter(_ != '\n').zipWithIndex.filter { case (c, i) => c == Map.coinSymbol }.unzip._2.map(Position.fromIndex(width)).toSet

    GameState(Map(width, height, coins), PlayerState(playerPos))
  }

  case class PlayerState(position: Position, score: Int = 0)
  case class GameState(map: Map, player: PlayerState, moves: Int = 100)

  sealed trait Direction
  case object North extends Direction
  case object South extends Direction
  case object East extends Direction
  case object West extends Direction

  object Direction {
    def fromChar(c: Char): Direction = c match {
      case 'w' => North
      case 'a' => West
      case 's' => South
      case 'd' => East
    }
  }

  def display(map: Map, player: PlayerState): String =
    map.display.updated(player.position.toIndex(map.width) + player.position.y, Map.playerSymbol)

  /**
   * Main functionality.
   */
  // These have been adapted from https://github.com/jdegoes/lambdaconf-2014-introgame
  def liftIO[A](io: IO[A]): Game[A] = StateT[IO, GameState, A](s => io.map(a => (s, a)))
  def state[A](f: GameState => (GameState, A)): Game[A] = StateT[IO, GameState, A](s => IO(f(s)))
  def readLineG: Game[String] = liftIO(IO.readLn)
  def printLineG(s: String): Game[Unit] = liftIO(IO.putStrLn(s))

  def gameLoop: Game[Unit] = for {
    c <- readLineG
    _ <- c.toList match {
      case 'q' :: _ => printLineG("Thanks for playing!")
      case c1 :: _ if "wasd".contains(c1) =>
        for {
          s <- movePlayer(Direction.fromChar(c.head))
          _ <- printLineG(display(s._3, s._4))
          _ <- printLineG(s"Moves: ${s._1}\tCoins: ${s._2}")
          _ <- if (s._1 == 0) {
            printLineG(s"Out of moves. You collected ${s._2} coins!")
          } else if (s._2 == startingState.map.coins.size) {
            printLineG(s"You collected all the coins in ${startingState.moves - s._1} moves. Congratulations!")
          } else gameLoop
        } yield ()
      case _ =>
        for {
          _ <- printLineG("Unknown input")
          _ <- gameLoop
        } yield ()
    }
  } yield ()

  val startingState = parse("%%%%%%%%%%\n%..$.....%\n%......$.%\n%...@....%\n%....$...%\n%.$......%\n%%%%%%%%%%")
  println("Welcome to rogue!")
  println(display(startingState.map, startingState.player))
  gameLoop.run(startingState).unsafePerformIO()
}
