import com.github.sardok.tictactoe._

object TestUtils {
  object VoidPlayer extends Player("Void Player", 'T') {
    override def play(board: Board) = None
  }

  def fillBoard(board: Board, positions: Pos*) = {
    val nboard = Board(board)
    positions map (pos => nboard.add(pos.x, pos.y, pos.symbol))
    nboard
  }

  /* Every symbol should be unique. */
  val scrambledBoard: Seq[Pos] = for {
    x <- 0 to 2
    y <- 0 to 2
  } yield Pos(x, y, (97 + ((x + 1) << y)).toChar)

}
