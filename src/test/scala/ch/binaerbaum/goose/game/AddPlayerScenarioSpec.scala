package ch.binaerbaum.goose.game

class AddPlayerScenarioSpec extends BaseAcceptance {

  info("As a player, I want to add me and others to the game so that we can play.")

  feature("Add players") {

    scenario("Add new players") {

      Given("there are no participants")
      val sm = init(add(Set()))

      val c1 = add(pippo)
      When(userWrites(c1))
      val onePlayer = sm.nextState(Command(c1))

      val (exp1, exp2) = (s"players: $pippo", s"players: $pippo, $pluto")
      Then(systemResponds(exp1))
      assert(onePlayer.msg === exp1)

      val c2 = add(pluto)
      When(userWrites(c2))
      val twoPlayers = init(onePlayer).nextState(Command(c2))

      Then(systemResponds(exp2))
      assert(twoPlayers.msg === exp2)

    }

    scenario("Duplicated players") {
      Given(s"there is already a participant $pippo")
      val onePlayer = init(add(Set(pippo)))

      val c = add(pippo)
      When(userWrites(c))
      val next = onePlayer.nextState(Command(c))

      val exp = s"$pippo: already existing player"
      Then(systemResponds(exp))
      assert(next.msg === exp)
    }
  }

}
