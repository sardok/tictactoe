package com.github.sardok.tictactoe
import scala.io.StdIn
import scala.util.Random
import scala.util.control.Exception._

abstract class Player(val name: String, val symbol: Char) {
  def play(board: Board): Option[(Int, Int)]
}

object Human extends Player("You", 'O') {
  def play(b: Board) = {
      (StdIn.readLine(s"Make your move as 'x, y' (symbol: $symbol): ") 
        split(",") map(s => toIntOpt(s.trim))).toList match {
        case Some(x: Int)::Some(y: Int)::_ =>
          Some((x, y))
        case _ => None
      }
  }

  def toIntOpt(s: String): Option[Int] = 
    catching(classOf[NumberFormatException]) opt s.toInt
}

object Machine extends Player("Computer", 'X') {
  /**
    * The AI is based on two actions, one is about
    * winning the game, the other one is about preventing
    * opponent from winning the game. These actions are
    * implemented by functions which are offensiveMove
    * and defensiveMove respectively.
    * Each function returns move(s) with weight value.
    * Weight value is used to make decision for the best move.
    */
  def play(board: Board) = {
    val (xmax, ymax) = (board.xmax, board.ymax)
    require(xmax == 3 && ymax == 3, 
      s"This machine does not know how to play in '$xmax x $ymax' board!")
    val board_axis = new BoardAxis(board)
    val scores = List(offensiveMove(board_axis), defensiveMove(board_axis))
    scores.flatten sortWith (_._1 > _._1) match {
      case ((_, pos: Pos))::_ => Some(pos.x, pos.y)
      case Nil => None
    }
  }

  private def scoreAxis(score_func: (Iterable[Pos] => Option[(Double, Pos)]))(board_axis: BoardAxis) = {
    /* Traverse each axis and score the moves.
     * Scoring is done based on a given function. 
     * Return the possible move with the highest score.
     */
    val rows = board_axis.yaxis
    val row_scores = for {
      row <- rows.values
    } yield score_func(row)

    val cols = board_axis.xaxis
    val col_scores = for {
      col <- cols.values
    } yield score_func(col)

    val scores: List[Option[(Double, Pos)]] = row_scores.toList ++ col_scores :+ score_func(board_axis.bslash) :+ score_func(board_axis.slash)
    // Eliminate the None's and reverse sort.
    scores.flatten sortWith (_._1 > _._1) match {
      case x::_ => Some(x)
      case Nil => None
    }
  }

  private def offensiveMove = scoreAxis { line => 
    val spaces = line filter (_.symbol == ' ')
    if (spaces.size > 0) {
      val played = line filter (_.symbol == symbol)
      val ratio = 1.0/3
      /* 0.1 increases favors offensive move in 
       * case of equality with defensive move. */
      Some(played.size * ratio + ratio + 0.1, (spaces.toList)(Random.nextInt(spaces.size)))
    } else {
      None
    }
  } _

  private def defensiveMove = scoreAxis { line =>
    /* Pretty much same with offensiveMove, scoring is different. */
    val spaces = line filter (_.symbol == ' ')
    val size = spaces.size
    if (spaces.size == 0) {
      None
    } else {
      val oponents = line filter (_.symbol != ' ') filter (_.symbol != symbol)
      val ratio = 1.0/3
      Some((ratio * oponents.size + ratio - 0.1, spaces.head))
    }
  } _
}
