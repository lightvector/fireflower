package fireflower

object Main {

  def main(args: Array[String]): Unit = {
    //println("Hello world!")
    //RandTest.test()

    val rules = Rules.StandardTwoPlayer

    // Sim.runSingle(
    //   rules = rules,
    //   players = (playerSeed => Array(
    //     new RandomPlayer(playerSeed,0,rules),
    //     new RandomPlayer(playerSeed,1,rules)
    //   )),
    //   doPrint = true,
    //   useAnsiColors = true
    // )

    val _games = Sim.runMulti(
      rules = rules,
      reps = 10000,
      runSeed = 0L,
      players = (playerSeed => Array(
        new RandomPlayer(playerSeed,0,rules),
        new RandomPlayer(playerSeed,1,rules)
      )),
      doPrint = true,
      doPrintDetails = false,
      useAnsiColors = true
    )

  }

}
