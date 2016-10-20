package fireflower

object Main {

  def main(args: Array[String]): Unit = {
    //println("Hello world!")
    //RandTest.test()

    val rules = Rules.Standard(numPlayers=2)

    // val _game = Sim.runSingle(
    //   rules = rules,
    //   players = (playerSeed => Array(
    //     new RandomPlayer(playerSeed,0,rules),
    //     new RandomPlayer(playerSeed,1,rules)
    //   )),
    //   doPrint = true,
    //   useAnsiColors = true
    // )

    // val _game = Sim.runSingle(
    //   rules = rules,
    //   gameSeed = -1905570214778309079L,
    //   playerSeed = 7246858482769613123L,
    //   players = Array(
    //     HeuristicPlayer(rules),
    //     HeuristicPlayer(rules)
    //   ),
    //   doPrint = true,
    //   useAnsiColors = true
    // )

    //TODO bombs are not counted in the eval
    //TODO the play sequencing is bad
    //TODO often run out of hints and then can't protect 5s
    //TODO look at why this went wrong
    //-6310882139785350969

    // val _game = Sim.runSingle(
    //   rules = rules,
    //   players = (playerSeed => Array(
    //     HeuristicPlayer(rules),
    //     HeuristicPlayer(rules)
    //   )),
    //   doPrint = true,
    //   useAnsiColors = true
    // )

    val _games = Sim.runMulti(
      rules = rules,
      reps = 100,
      runSeed = 1L,
      players = (playerSeed => Array(
        HeuristicPlayer(rules),
        HeuristicPlayer(rules)
      )),
      doPrint = true,
      doPrintDetails = false,
      useAnsiColors = true
    )

    // val _games = Sim.runMulti(
    //   rules = rules,
    //   reps = 10000,
    //   runSeed = 0L,
    //   players = (playerSeed => Array(
    //     new RandomPlayer(playerSeed,0,rules),
    //     new RandomPlayer(playerSeed,1,rules)
    //   )),
    //   doPrint = true,
    //   doPrintDetails = false,
    //   useAnsiColors = true
    // )

  }

}
