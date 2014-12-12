package com.github.sardok.tictactoe
case class Play(board: Board)
case object Played
case object StartGame
case class EndGame(message: String)
case class KeyboardInput(bytes: String)
case class Move(x: Int, y: Int, name: String, symbol: Char)