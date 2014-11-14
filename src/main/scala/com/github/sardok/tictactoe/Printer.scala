package com.github.sardok.tictactoe
import Array._

abstract class Printer {
  def print(booard: Board)
  def print(str: String)
}

class ConsolePrinter extends Printer {

  def print(board: Board) = {
    val layout = new GridLayout(board.xmax, board.ymax)
    board map (pos => layout.set(pos.x, pos.y, pos.symbol))
    layout.print(Predef.print)
    println()
  }

  def print(str: String) = println(str)
}
