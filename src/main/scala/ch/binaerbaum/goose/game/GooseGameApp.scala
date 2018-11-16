package ch.binaerbaum.goose.game

import scala.io.StdIn

object GooseGameApp extends App {

  // Stream of user commands (inputs)
  private lazy val userCommands: Stream[Command] = Stream.continually(StdIn.readLine()).map(in => Command(in))

  private def isEnded(s: GameState) = s match {
    case e: Ended => true
    case _ => false
  }

  // Initialize the stream and generate the state transitions based on user commands
  new GameStates(userCommands).stream
    .takeWhile(s => !isEnded(s))
    .map(_.msg)
    .foreach(println)
}
