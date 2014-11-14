import com.github.sardok.tictactoe._
import org.scalatest._
import TestUtils._

class GameSpec extends FlatSpec with Matchers {

  def successfulGame(positions: Pos*) = {
    val board = fillBoard(new Board(3, 3), positions: _*)
    val player = VoidPlayer
    val game = new Game(player, player, board)
    a [PlayerWonException] should be thrownBy {
      game.checkGameState(board)
    }
  }

  /** Because i failed to parameterize exception type here,
    * this function is kinda duplication of successfulGame.
    */
  def failedGame(positions: Pos*) = {
    val board = fillBoard(new Board(3, 3), positions: _*)
    val player = VoidPlayer
    val game = new Game(player, player, board)
    a [GameFailed] should be thrownBy {
      game.checkGameState(board)
    }
  }

  "Win the game at X axis" should behave like successfulGame(Pos(0, 0, 'T'), Pos(1, 0, 'T'), Pos(2, 0, 'T'))

  "Win the game at Y axis" should behave like successfulGame(Pos(0, 0, 'T'), Pos(0, 1, 'T'), Pos(0, 2, 'T'))

  "Win the game at BackSlash axis" should behave like successfulGame(Pos(0, 0, 'T'), Pos(1, 1, 'T'), Pos(2, 2, 'T'))

  "Win the game at Slash axis" should behave like successfulGame(Pos(2, 0, 'T'), Pos(1, 1, 'T'), Pos(0, 2, 'T'))

  "A Game should fail when there is no space on board" should behave like failedGame(scrambledBoard: _*)

  it should "not finish if a row or col is filled" in {
    val board = fillBoard(new Board(3, 3), Pos(0, 0, 'O'), Pos(1, 0, 'T'), Pos(2, 0, 'O'))
    val player = VoidPlayer
    val game = new Game(player, player, board)
    assert(() === game.checkGameState(board))
  }
}
