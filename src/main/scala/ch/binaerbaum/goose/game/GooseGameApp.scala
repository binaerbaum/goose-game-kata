package ch.binaerbaum.goose.game

import scala.io.StdIn

object GooseGameApp extends App {

  // Stream of user commands (inputs)
  lazy val userCommands: Stream[Command] = Stream.continually(StdIn.readLine()).map(in => Command(in))

  val endedPf: PartialFunction[GameState, Boolean] = {
    case Ended(_) => true
  }

  def isEnded(s: GameState) = endedPf.isDefinedAt(s)

  // Initialize the state and generate the state transitions based on user commands
  new GameStates(userCommands).stream
    .takeWhile(s => !isEnded(s))
    .map(_.msg)
    .foreach(println)
}
