package ch.binaerbaum.goose.game

import ch.binaerbaum.goose.game.GameBoard.{End, Square, Start}

class MovePlayerScenarioSpec extends BaseAcceptance {

  info("As a player, I want to move the marker on the board to make the game progress")

  feature("Move a player") {

    scenario("Start") {

      val curPos = Start.index
      Given(s"""there are two participants "$pippo" and "$pluto" on space "${Start.name}" """)
      val sm = init(playing(Map(pippo -> curPos, pluto -> curPos)))

      val (dr1, c1) = rollMv(pippo, DiceRoll(4, 3))
      When(userWrites(c1))
      val mv1 = sm.nextState(Command(c1))

      val exp1 = s"$pippo rolls $dr1. $pippo moves from ${Start.name} to ${dr1.sum}"
      Then(systemResponds(exp1))
      assert(mv1.msg === exp1)

      val (dr2, c2) = rollMv(pluto, DiceRoll(2, 2))
      When(userWrites(c2))
      val mv2 = init(mv1).nextState(Command(c2))

      val exp2 = s"$pluto rolls $dr2. $pluto moves from ${Start.name} to ${dr2.sum}"
      Then(systemResponds(exp2))
      assert(mv2.msg === exp2)

    }
  }

  info("As a player, I win the game if I land on space 63")

  feature("Win") {
    val curPos = 60

    scenario("Victory") {
      Given(s"""there is one participant "$pippo" on space "$curPos"""")
      val sm = init(playing(Map(pippo -> curPos)))

      val (dr1, c1) = rollMv(pippo, DiceRoll(1, 2))
      When(userWrites(c1))
      val mv1 = sm.nextState(Command(c1))

      val exp = s"$pippo rolls $dr1. $pippo moves from $curPos to ${curPos + dr1.sum}. $pippo Wins!!"
      Then(systemResponds(exp))
      assert(mv1.msg === exp)
    }


    scenario("Winning with the exact dice shooting") {
      Given(s"""there is one participant "$pippo" on space "$curPos"""")
      val sm = init(playing(Map(pippo -> curPos)))

      val (dr, c) = rollMv(pippo, DiceRoll(3, 2))
      When(userWrites(c))
      val next = sm.nextState(Command(c))

      val endPos = End.index - (curPos + dr.sum - End.index)
      val exp = s"$pippo rolls $dr. $pippo moves from $curPos to ${End.name}. $pippo bounces! $pippo returns to $endPos"
      Then(systemResponds(exp))
      assert(next.msg === exp)

    }
  }

  info("As a player, I want the game throws the dice for me to save effort")

  feature("The game throws the dice") {
    scenario("Dice roll") {
      val curPos = 4

      Given(s"""there is one participant "$pippo" on space "$curPos"""")
      val sm = init(playing(Map(pippo -> curPos)))

      val (dr, c) = (DiceRoll(1, 2), move(pippo))
      When(s"""when the user writes: "$c" (assuming that the dice get $dr)""")
      val next = sm.nextState(mvWithExpRoll(c, dr))

      val exp = s"$pippo rolls $dr. $pippo moves from $curPos to ${curPos + dr.sum}"
      Then(systemResponds(exp))
      assert(next.msg === exp)
    }
  }

  info("""As a player, when I get to the space "The Bridge", I jump to the space "12"""")

  feature("""Space "6" is "The Bridge"""") {
    scenario(s"""Get to "The Bridge"""") {

      val curPos = 4

      Given(s"""there is one participant "$pippo" on space "$curPos"""")
      val sm = init(playing(Map(pippo -> curPos)))

      val (dr, c) = (DiceRoll(1, 1), move(pippo))
      When(s"""when the user writes: "$c" (assuming that the dice get $dr)""")
      val next = sm.nextState(mvWithExpRoll(c, dr))

      val ex = s"$pippo rolls $dr. $pippo moves from $curPos to ${Square(curPos + dr.sum).name}. $pippo jumps to 12"
      Then(systemResponds(ex))
      assert(next.msg === ex)

    }
  }

  info("""As a player, when I get to a space with a picture of "The Goose", I move forward again by the sum of the two dice rolled before""")
  info("""The spaces 5, 9, 14, 18, 23, 27 have a picture of "The Goose"""")

  feature("""If you land on "The Goose", move again""") {
    scenario("Single Jump") {
      val curPos = 3
      val endPos = 7

      Given(s"""there is one participant "$pippo" on space "$curPos"""")
      val sm = init(playing(Map(pippo -> curPos)))

      val (dr, c) = (DiceRoll(1, 1), move(pippo))
      When(s"""${userWrites(c)} (assuming that the dice get $dr)""")
      val next = sm.nextState(mvWithExpRoll(c, dr))

      val exp = s"$pippo rolls $dr. $pippo moves from $curPos to ${Square(curPos + dr.sum).name}. $pippo moves again and goes to $endPos"
      Then(systemResponds(exp))
      assert(next.msg === exp)

    }

    scenario("Multiple Jumps") {
      val curPos = 10
      val endPos = 22

      Given(s"""there is one participant "$pippo" on space "$curPos"""")
      val sm = init(playing(Map(pippo -> curPos)))

      val (dr, c) = (DiceRoll(2, 2), move(pippo))
      When(s"""${userWrites(c)} (assuming that the dice get $dr)""")
      val next = sm.nextState(mvWithExpRoll(c, dr))

      val exp = s"$pippo rolls $dr. $pippo moves from $curPos to ${Square(curPos + dr.sum).name}. $pippo moves again and goes to ${Square(curPos + 2 * dr.sum ).name}. $pippo moves again and goes to $endPos"
      Then(systemResponds(exp))
      assert(next.msg === exp)

    }

  }
}
