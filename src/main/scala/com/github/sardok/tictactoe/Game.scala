package com.github.sardok.tictactoe
import scala.util.control.Exception._

case class StopGameException(msg: String) extends RuntimeException(msg)
case class PlayerWonException() extends RuntimeException("")
case class GameFailed() extends RuntimeException("")

class Game(p1: Player, p2: Player, board: Board, printer: Printer) {
  /* This game is restricted to 3x3 board. */
  val players = List(p1, p2)
  val player_iter: Iterator[Player] = new Iterator[Player] {
    var i = -1
    def hasNext = true
    def next(): Player = {
      i = (i + 1) % 2
      players(i)
    }
  }

  def step = {
    val player = player_iter.next()
    val name = player.name
    player play board match {
      case Some((x: Int, y: Int)) => 
        printer.print(s"$name played as x: $x, y: $y.")
        board add (x, y, player.symbol)
        try {
          checkGameState(board)
        } catch {
          case ex: PlayerWonException =>
            throw new StopGameException(s"$name won!")
          case ex: GameFailed =>
            throw new StopGameException("No winner. Game is finished!")
        }
      case _ => printer.print("Invalid move!, your turn is passed!")
    }

  }

  def checkGameState(board: Board) = {
    if (isSucceded(board)) {
      throw new PlayerWonException
    }
    if (isFailed(board)) {
      throw new GameFailed
    }
  }

  def isSucceded(board: Board) = {
    def axis_predicate(it: Iterable[Pos]) = {
      val nonempty = it filter (_.symbol != ' ')
      val grouped = nonempty groupBy (_.symbol)
      if (grouped.values exists (_.size == 3)) {
        true
      } else {
        false
      }
    }
    val baxis = new BoardAxis(board)
    if ((baxis.all map axis_predicate) exists (_ == true)) {
      true
    } else {
      false
    }
  }

  def isFailed(board: Board) = {
    def axis_predicate(it: Iterable[Pos]) = {
      val nonempty = it filter (_.symbol != ' ')
      val grouped = nonempty groupBy (_.symbol)
      if ((grouped.values map (_.size)).sum == 3) {
        true
      } else {
        false
      }
    }
    val baxis = new BoardAxis(board)
    if ((baxis.all map axis_predicate) forall (_ == true)) {
      true
    } else {
      false
    }
  }

  var isEnd = false

  def end = {
    isEnd = true
  }
}
