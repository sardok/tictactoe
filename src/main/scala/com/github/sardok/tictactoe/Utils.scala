package com.github.sardok.tictactoe

object Utils {
  def and[T](ps: (T => Boolean)*) = (e: T) => ps forall(_(e))


}
