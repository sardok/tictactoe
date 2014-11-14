import com.github.sardok.tictactoe._
import org.scalatest._
import org.scalatest.Assertions._
import TestUtils._

class AISpec extends FlatSpec with Matchers {

  def expectedMove(expectation: Option[(Int, Int)], positions: Pos*) = {
    val machine = Machine
    val board = fillBoard(new Board(3, 3), positions: _*)
    val move = machine play (board)
    assert(move === expectation)
  }

  "Winning move at Y axis" should behave like expectedMove(Some((1, 0)), Pos(0, 0, Machine.symbol), Pos(2, 0, Machine.symbol))

  "Winning move at X axis" should behave like expectedMove(Some((1, 0)), Pos(1, 2, Machine.symbol), Pos(1, 1, Machine.symbol))

  "Winning move at BackSlash axis" should behave like expectedMove(Some((0, 0)), Pos(2, 2, Machine.symbol), Pos(1, 1, Machine.symbol))

  "Winning move at Slash axis" should behave like expectedMove(Some((1, 1)), Pos(2, 0, Machine.symbol), Pos(0, 2, Machine.symbol))

  "Prevent oponent from winning at Y axis" should behave like expectedMove(Some((1, 0)), Pos(0, 0, 'O'), Pos(2, 0, 'O'))

  "Prevent oponent from winning at X axis" should behave like expectedMove(Some((1, 0)), Pos(1, 2, 'O'), Pos(1, 1, 'O'))

  "Prevent oponent from winning at BackSlash axis" should behave like expectedMove(Some((0, 0)), Pos(2, 2, 'O'), Pos(1, 1, 'O'))

  "Prevent oponent from winning at Slash axis" should behave like expectedMove(Some((1, 1)), Pos(2, 0, 'O'), Pos(0, 2, 'O'))

  "Favor winning over preventing" should behave like expectedMove(Some((0, 2)), 
    Pos(0, 0, Machine.symbol), Pos(0, 1, Machine.symbol), Pos(2, 0, 'O'), Pos(2, 2, 'O'))

  "Pass the turn if board is invalid" should behave like expectedMove(None, scrambledBoard: _*)
}
