package com.github.sardok.tictactoe
import akka.actor.{Props, ActorSystem}


object TicTacToe extends App {

  val system = ActorSystem("tictactoe")
  val coordinator = system.actorOf(Props[Coordinator])

  coordinator ! StartGame

  system.awaitTermination()

}
