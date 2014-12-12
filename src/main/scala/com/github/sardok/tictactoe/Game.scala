package com.github.sardok.tictactoe
import scala.util.control.Exception._
import scala.language.reflectiveCalls
import akka.actor.{Actor, Props, ActorRef}

case class StopGameException(msg: String) extends RuntimeException(msg)
case class PlayerWonException() extends RuntimeException("")
case class GameFailed() extends RuntimeException("")

class Game(p1: ActorRef, p2: ActorRef, board: Board, printer: Printer = new ConsolePrinter) extends Actor {
  /* This game is restricted to 3x3 board. */
  val players = List(p1, p2)

  val player_iter = new Iterator[ActorRef] {
    var i = -1
    def hasNext = true
    def next() = {
      i = (i + 1) % 2
      players(i)
    }
  }

  def receive = {
    case StartGame =>
      printer.print(board)
      player_iter.next() ! Play(board)
    case input: KeyboardInput =>
      // Pass the message to the current player.
      val player = players(player_iter.i)
      player ! input
    case Move(x, y, name, symbol) =>
      printer.print(s"$name played as x: $x, y: $y.")
      board add (x, y, symbol)
      printer.print(board)
      try {
        checkGameState(board)
        player_iter.next() ! Play(board)
      } catch {
        case ex: PlayerWonException =>
          println("Game is ended")
          context.parent ! EndGame(s"$name won!")
        case ex: GameFailed =>
          println("Game is failed.")
          context.parent ! EndGame("No winner. Game is finished!")
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
}

object Game {
  def props(p1: ActorRef, p2: ActorRef, board: Board, printer: Printer) = Props(classOf[Game], p1, p2, board, printer)
}