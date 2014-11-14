package com.github.sardok.tictactoe

class Canvas(val width: Int, val height: Int, val cell_size: Int) {
  val buffer = Array.ofDim[Char](height * cell_size, width * cell_size)
  var cursor_y = 0
  var cursor_x = 0

  def draw(grid: Grid) = {
    val tmp_buff = Array.ofDim[Char](cell_size, cell_size)
    grid.write_to_buffer(tmp_buff)
    for {
      y <- 0 until cell_size
      x <- 0 until cell_size
    } {
      buffer(cursor_y + y)(cursor_x + x) = tmp_buff(y)(x)
    }

    /* Update cursor position. */
    cursor_x += cell_size
    if (cursor_x % buffer(0).size == 0) {
      cursor_x = 0
      cursor_y += cell_size
    }
  }

  override def toString = {
    val rows = for {
      i <- 0 until buffer.size
    } yield buffer(i) mkString ""
    rows mkString "\n"
  }
}
