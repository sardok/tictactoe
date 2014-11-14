package com.github.sardok.tictactoe

case class Pos(x: Int, y: Int, symbol: Char = ' ') {
  override def toString() = symbol.toString
}

class Board(val xmax: Int = 3, val ymax: Int = 3) extends Iterable[Pos] {
  var layout = for {
    x <- 0 until xmax
    y <- 0 until ymax
  } yield Pos(x, y)

  def add(x: Int, y: Int, symbol: Char, force: Boolean = false) = {
    layout = layout map { case p: Pos => 
      if (p.x == x && p.y == y && (p.symbol == ' ' || force)) 
        Pos(x, y, symbol) else p }
  }

  override def iterator = layout.iterator
}

object Board {
  def apply(board: Board) = {
    val nboard = new Board(board.xmax, board.ymax)
    nboard.layout = board.layout
    nboard
  }

}

class BoardAxis(board: Board) {
  val yaxis: Map[Int, Iterable[Pos]] = board groupBy (_.y)
  val xaxis: Map[Int, Iterable[Pos]] = board groupBy (_.x)
  val bslash: Iterable[Pos] = board filter (p => p.x == p.y)
  val middlex: Int = board.xmax / 2
  val slash: Iterable[Pos] = bslash map { e =>
    /* Take mirror to the x=middlex (which is 1 highly likely) line. */
    val x = middlex + (middlex - e.x)
    val y = e.y
    board find (p => p.x == x && p.y == y) match {
      case Some(point) => point
      case None => throw new Exception("Cannot find slash line of the board!")
    }
  }
  val all = (yaxis.values).toList ++ xaxis.values :+ slash :+ bslash
}

