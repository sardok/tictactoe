package com.github.sardok.tictactoe
import akka.camel.{CamelMessage, Consumer}

class StdIn extends Consumer {
  def endpointUri = "//stream:in"

  def receive = {
    case msg: CamelMessage =>
      val body = msg.bodyAs[String]
      context.parent ! new KeyboardInput(body)
    case _ => ()
  }
}
