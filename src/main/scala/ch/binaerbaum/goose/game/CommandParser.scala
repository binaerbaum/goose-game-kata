package ch.binaerbaum.goose.game

import scala.util.Random
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * Parser for user input and their translation to commands
  * @param rollDice supplier of random values for the dice
  */
case class CommandParser(rollDice: () => DiceRoll) extends JavaTokenParsers {

  /**
    * Parses the user input and translates it to a Command
    * @param input the command input by the user
    * @return a Command matching the input or CommandNotUnderstood
    */
  def parse(input: String): Command = parseAll(command, input) match {
    case Success(matched, _) => matched
    case Failure(msg, _) => CommandNotUnderstood(input, msg)
    case Error(msg, _) => CommandNotUnderstood(input, msg)
  }

  // Parse dice rolls
  private def diceRoll: Parser[Int] = "[1-6]".r ^^ (_.toInt)

  private def doubleDiceRoll: Parser[DiceRoll] = diceRoll ~ "," ~ diceRoll ^^ {
    case d1 ~ _ ~ d2 => DiceRoll(d1, d2)
  }

  // Parses add player <name>
  private def addPlayer: Parser[Command] = "add player" ~ ident ^^ {
    case _ ~ name => AddPlayer(name)
  }

  // Parses move <name> or move <name> d1, d2
  private def movePlayer: Parser[Command] = "move" ~ ident ~ opt(doubleDiceRoll) ^^ {
    case _ ~ name ~ diceRoll => diceRoll match {
      case Some(roll) => MovePlayer(name, roll)
      case None => MovePlayer(name, rollDice())
    }
  }

  // Quit command
  private def quit: Parser[Command] = "quit".r ^^ (_ => Quit)

  // Combinator
  private def command: Parser[Command] = addPlayer | movePlayer | quit

}

// A double dice roll
case class DiceRoll(first: Int, second: Int) {
  def sum: Int = first + second

  override def toString: String = s"$first, $second"
}

// Commands that represent parsed user input
sealed trait Command

case class MovePlayer(pn: String, dr: DiceRoll) extends Command

case class AddPlayer(pn: String) extends Command

// Used when the parsing fails
case class CommandNotUnderstood(in: String, err: String) extends Command

case object Quit extends Command

object Command {
  // Rolls a dice for the case where none provided
  private def rand6: Int = Random.nextInt(6) + 1

  private def rollDice() = DiceRoll(rand6, rand6)

  def apply(in: String): Command = CommandParser(() => rollDice()).parse(in)
}