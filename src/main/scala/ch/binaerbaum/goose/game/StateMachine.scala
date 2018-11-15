package ch.binaerbaum.goose.game

import ch.binaerbaum.goose.game.GameBoard._

import scala.annotation.tailrec

/**
  * Manages the transitions from one state of the game to another.
  *
  * @param s the source state from which the transition will occur
  */
case class StateMachine(s: GameState) {

  /**
    * Defines the transition rules from the source state. Invalid transitions are modelled by an Error state.
    * The system can recover from an error.
    *
    * @param c the command that triggered the state change
    * @return the allowed target state transition or an error state
    */
  def nextState(c: Command): GameState = {

    def generatePlayerMap(pns: Set[String]) = pns.map(pn => (pn, Player(pn, Start))).toMap

    // println(s"state: $s, command: $c") // Uncomment to debug state transitions
    (s, c) match {
      case (Starting, AddPlayer(name)) => addPlayer(name, Set[String]())
      case (AddingPlayers(pns, _), AddPlayer(n)) => addPlayer(n, pns)
      case (AddingPlayers(pns, _), MovePlayer(p, dr)) => movePlayer(p, generatePlayerMap(pns), dr)
      case (Playing(ps, _), MovePlayer(pn, dr)) => movePlayer(pn, ps, dr)
      case (_, Quit) | (Ending(_), _) => Ended()
      case (Error(prev, _), com) => StateMachine(prev).nextState(com)
      case (_, CommandNotUnderstood(input, _)) => Error(s, s"Input [$input] not understood.")
      case _ => Error(s, s"Illegal transition from current state !")
    }
  }

  // Computes the new target state after a given player is added
  private def addPlayer(pn: String, ps: Set[String]): GameState = if (ps.contains(pn)) {
    Error(s, s"$pn: already existing player")
  } else {
    val np = ps + pn
    AddingPlayers(np, s"""players: ${np.mkString(", ")}""")
  }

  // Computes the new target state after a given player moves with a dice roll
  private def movePlayer(pn: String, ps: Map[String, Player], dr: DiceRoll): GameState = {

    def mapToState(mv: Move, msg: String): GameState = mv.to match {
      case End => Ending(msg)
      case square => Playing(ps + (pn -> Player(pn, square)), msg)
    }

    // Get the end position of the player and compute the state from it
    val res = for {
      p <- ps.get(pn)
      mvs = move(p.position, dr)
      mv <- mvs.headOption
    } yield mapToState(mv, genMsg(pn, mvs, dr))

    res.getOrElse(Error(s, s"Player $pn not found"))
  }

  // Computes the list of moves (transitions from squares) a player makes for a given dice roll
  private def move(src: Square, dr: DiceRoll): List[Move] = {
    @tailrec
    def inner(acc: List[Move]): List[Move] = acc match {
      case x :: xs => x.to match {
        case b@Bridge => Move(b, Square(b.target)) :: acc
        case g@Goose(index) => inner(Move(g, Square(index + dr.sum)) :: acc)
        case RegularSquare(_) | End => acc
        case Start => throw new IllegalStateException("Cannot go back to Start")
      }
      case _ => acc
    }

    inner(nextSquares(src, dr.sum))
  }


  private def nextSquares(src: Square, mv: Int): List[Move] = {
    val nextIdx = src.index + mv
    val endIdx = End.index

    if (nextIdx <= endIdx) {
      Move(src, Square(nextIdx), nextIdx == endIdx) :: Nil
    } else {
      val bounceIdx = endIdx - (nextIdx - endIdx)
      Move(End, Square(bounceIdx)) :: Move(src, End) :: Nil
    }
  }

  // Returns the text to be output corresponding to a series of moves by a given player
  private def genMsg(pn: String, mvs: List[Move], dr: DiceRoll): String = {

    // Returns the message associated with a given game situation (Rule)
    def msg(mv: Move): String = (mv.from, mv.to, mv.win) match {
      case (from, End, win) if win => s"$pn moves from ${from.name} to ${End.name}. $pn Wins!!"
      case (End, sq, _) => s"$pn bounces! $pn returns to ${sq.name}"
      case (Bridge, to, _) => s"$pn jumps to ${to.name}"
      case (Goose(_), to, _) => s"$pn moves again and goes to ${to.name}"
      case (from, to, _) => s"$pn moves from ${from.name} to ${to.name}"
    }

    val intro = s"$pn rolls $dr"

    mvs.map(mv => msg(mv)).foldRight(intro)((m, acc) => s"$acc. $m")
  }

}
