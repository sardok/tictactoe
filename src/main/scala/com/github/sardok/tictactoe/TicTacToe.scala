package com.github.sardok.tictactoe

object TicTacToe extends App {

  val board = new Board(3, 3)
  val printer = new ConsolePrinter
  val game = new Game(Human, Machine, board, printer)

  getChar("Welcome to TicTacToe, press any key to start . . .")

  while (!game.isEnd) {
    printer.print(board)
    try {
      game.step
    } catch {
      case StopGameException(msg) =>
        printer.print(msg)
        printer.print(board)
        game.end
    }
  }

  def getChar(msg: String = "") = {
    if (!msg.isEmpty) {
      printer.print(msg)
    }
    Console.in.read.toChar
  }
}
