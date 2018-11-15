package ch.binaerbaum.goose.game

import ch.binaerbaum.goose.game.GameBoard.Square

/**
  * The state of the player on the board.
  *
  * @param name     the player's name
  * @param position the player's current position
  */
case class Player(name: String, position: Square)

/**
  * A move (transition) from one square to another
  * @param from the square from which the player is moving
  * @param to the new square towards which the player is moving
  * @param win indicates if the move is a win
  */
case class Move(from: Square, to: Square, win: Boolean = false)
