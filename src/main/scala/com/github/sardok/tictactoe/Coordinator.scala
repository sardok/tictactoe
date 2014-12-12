package com.github.sardok.tictactoe
import akka.actor.{Actor, Props}

class Coordinator extends Actor {

  val board = new Board(3, 3)
  val printer = new ConsolePrinter
  val p1 = context.actorOf(Human.props, "player1")
  val p2 = context.actorOf(Machine.props, "player2")
  val game = context.actorOf(Game.props(p1, p2, board, printer), "game")
  val input = context.actorOf(Props[StdIn])

  def receive = {
    case StartGame => game ! StartGame
    case evt: KeyboardInput => game ! evt
    case EndGame(msg) =>
      printer.print(msg)
      context.system.shutdown
  }
}