name := "TicTacToe"

version := "0.1"

scalaVersion := "2.11.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.6"

libraryDependencies += "com.typesafe.akka" %% "akka-camel" % "2.3.6"

libraryDependencies += "org.apache.camel" % "camel-stream" % "1.3.0"

scalacOptions ++= Seq("-deprecation", "-feature")
