package ch.binaerbaum.goose.game

import ch.binaerbaum.goose.game.GameBoard.Square
import org.scalatest.{FeatureSpec, GivenWhenThen}

abstract class BaseAcceptance extends FeatureSpec with GivenWhenThen {

  val (pippo, pluto) = ("Pippo", "Pluto")

  def init(s: GameState) = StateMachine(s)

  def add(pns: Set[String]) = AddingPlayers(pns, "")

  def playing(ps: Map[String, Int]) = Playing(ps.map{case (k, v) => k -> Player(k, Square(v))}, "")

  def add(pn: String) = s"add player $pn"

  val roll = DiceRoll(1, 4)

  def rollMv(pn: String, dr: DiceRoll): (DiceRoll, String) = (dr, move(pn, dr))

  def move(pn: String, dr: DiceRoll) = s"move $pn $dr"
  def move(pn: String) = s"move $pn"

  // Move with expected roll. Provides the "random" roll to the parser
  def mvWithExpRoll(c: String, expRoll: DiceRoll): Command = CommandParser(() => expRoll).parse(c)

  def userWrites(msg: String) = s"""the user writes: "$msg" """

  def systemResponds(msg: String) = s"""the system responds: "$msg""""

}
