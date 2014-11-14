package com.github.sardok.tictactoe

object GridPosition extends Enumeration {
  type GridPosition = Value
  val Left, TopLeft, Top, TopRight, Right, BottomRight, Bottom, BottomLeft, Middle = Value
}
import GridPosition._

/**
  * A single unit in the layout. By default it is 3 by 3 square.
  * The data will be represented in 1,1 (in the middle).
  * Rest of spaces will be used for border lines.
  */
class Grid(val pixel: Int = 3) {
  val data = Array.fill(pixel, pixel)(' ')
  val default = data(0)(0)
  val topleft = '\u231C'
  val topright = '\u231D'
  val bottomleft = '\u231E'
  val bottomright = '\u231F'
  val upperline = '\u2594'
  val lowerline = '\u2581'
  val leftline = '\u258F'
  val rightline = '\u2595'

  def add_border(position: GridPosition) = position match {
    case Top =>
      data(0) = data(0) map (_ => upperline)
    case Bottom =>
      data(2) = data(2) map (_ => lowerline)
    case Left =>
      data(0)(0) = leftline
      data(1)(0) = leftline
      data(2)(0) = leftline
    case Right =>
      data(0)(2) = rightline
      data(1)(2) = rightline
      data(2)(2) = rightline
  }

  def default_or_alt(existing: Char, expected: Char, default: Char, alternative: Char) = existing match {
    case ch: Char if ch == expected => default
    case _ => alternative
  }

  def write_to_buffer(buffer: Array[Array[Char]]) = {
    val bsize = buffer.size
    val tsize = buffer.foldLeft(0)((a, e) => a + e.size) 
    assume(bsize == pixel, s"Invalid buffer size: $bsize")
    assume(tsize == pixel * pixel, s"Invalid buffer total size: $tsize")

    for {
      x <- 0 until pixel
      y <- 0 until pixel
    } {
      buffer(y)(x) = data(y)(x)
    }
  }

  def set_value(value: Char, position: GridPosition = Middle) = {
    val (r, c) = position_to_coordinate(position)
    data(r)(c) = value
  }

  def get_value(position: GridPosition = Middle) = {
    val (r, c) = position_to_coordinate(position)
    data(r)(c)
  }

  def position_to_coordinate(position: GridPosition) = position match {
    case Left => (1, 0)
    case TopLeft => (0, 0)
    case Top => (0, 1)
    case TopRight => (0, 2)
    case Right => (1, 2)
    case BottomRight => (2, 2)
    case Bottom => (2, 1)
    case BottomLeft => (2, 0)
    case Middle => (1, 1)
  }
}

/**
  * A checkerboard layout. Every entity (square) is made of a Grid.
  */
class GridLayout(xmax: Int, ymax: Int) {

  val row = ymax + 2
  val col = xmax + 2
  val grids = Array.ofDim[Grid](row, col)
  for {
    r <- 0 until row
    c <- 0 until col
  } grids(r)(c) = new Grid

  /*
   * Draw lines to show the grid layout.
   * Note that last col and row is deliberately ignored to be used for labeling.
   */
  for {
    r <- 0 until (row - 1)
    c <- 0 until (col - 1)
  } grids(r)(c).add_border(Bottom)

  for {
    r <- 0 until (row - 1)
    c <- 0 until (col - 1)
  } grids(r)(c).add_border(Right)

  /* Set x coordinate labels. */
  for {
    i <- 0 until (new Grid).pixel
  } grids(0)(i + 1).set_value(('0' + i).toChar)

  /* Set y coordinate labels. */
  for {
    i <- 0 until (new Grid).pixel
  } grids(i + 1)(0).set_value(('0' + i).toChar)

  /* Set coordinate indicator labels. */
  grids(row / 2)(col - 1).set_value('Y')
  grids(row - 1)(2).set_value('X')

  val canvas = new Canvas(row, col, (new Grid).pixel)

  def print(printer: Any => Unit) = {
    for {
      r <- 0 until row
      c <- 0 until col
    } canvas.draw(grids(r)(c))
    printer(canvas)
  }

  def set(x: Int, y: Int, data: Char) = grids(y + 1)(x + 1).set_value(data)
}
