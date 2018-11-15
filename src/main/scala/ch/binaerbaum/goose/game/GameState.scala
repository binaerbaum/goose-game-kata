package ch.binaerbaum.goose.game

sealed trait GameState {
  def msg: String
}

// The initial state of the game, before players are added
case object Starting extends GameState {
  override def msg: String =
    """Type "add player <name>" to add a player.
      |Type "move <name> 3, 4" to move the player with a specific dice roll.
      |Type "move <name> if you want the system to move the player.
      |Type "quit" at any moment to quit the game.
      |Have Fun!""".stripMargin
}

/**
  * The state where players can be added
  *
  * @param pns the set of player names
  * @param msg the message to be output when entering this state
  */
case class AddingPlayers(pns: Set[String], msg: String) extends GameState

/**
  * The main state of the game, where players can be moved.
  *
  * @param ps  map of players that keeps their positions up to date
  * @param msg the message to be output when entering this state
  */
case class Playing(ps: Map[String, Player], msg: String) extends GameState

// Transition state after a player has won
case class Ending(msg: String) extends GameState

// Final state of the game
case class Ended(msg: String = "Bye!") extends GameState

/**
  * A state that represents an error from either a bad input or an illegal transition.
  *
  * @param prev a reference to the last non error known state
  * @param msg  the error message to be displayed when entering this state
  */
case class Error(prev: GameState, msg: String) extends GameState

/**
  * Generates a (potentially infinite) lazily evaluated stream of states transitions.
  * The transitions occur each time a new command comes in on the command stream.
  * The latter is passed by name to its head being evaluated before outputing the Starting stream
  *
  * @param cmds the stream of user commands (inputs)
  */
class GameStates(cmds: => Stream[Command]) {

  val stream: Stream[GameState] = Starting #:: cmds.zip(stream).map {
    case (c, s) => nextState(s, c)
  }

  private def nextState(s: GameState, c: Command): GameState = StateMachine(s).nextState(c)

}
