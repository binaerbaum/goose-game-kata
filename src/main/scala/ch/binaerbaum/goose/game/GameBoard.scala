package ch.binaerbaum.goose.game

/**
  * Container for the consituents of the game board, i.e. the squares
  */
object GameBoard {

  // Represents a square of the game board
  sealed trait Square {
    def index: Int

    def name: String = index.toString
  }

  // Starting square
  case object Start extends Square {
    override def index: Int = 0

    override def name: String = "Start"
  }

  // A non special square
  case class RegularSquare(index: Int) extends Square

  // Special square The Goose
  case class Goose(index: Int) extends Square {
    override def name: String = s"$index, The Goose"
  }

  // Special square The Bridge
  case object Bridge extends Square {
    override def index: Int = 6
    override def name: String = "The Bridge"
    def target: Int = 12
  }

  // The last square
  case object End extends Square {
    override def index: Int = 63
  }

  object Square {
    def apply(position: Int): Square = {
      position match {
        case 0 => Start
        case 6 => Bridge
        case 5 | 9 | 14 | 18 | 23 | 27 => Goose(position)
        case 63 => End
        case _ if position < 63 => RegularSquare(position)
        case _ => throw new IllegalArgumentException("Cannot have a square beyond 63")
      }
    }
  }

}
