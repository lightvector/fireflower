package fireflower

object Main {

  def main(args: Array[String]): Unit = {
    //println("Hello world!")
    //RandTest.test()
    val rand = Rand()
    //val gameSeed = rand.nextLong()
    val gameSeed = 8411726411147478630L
    val playerSeed = gameSeed
    println("GameSeed: " + gameSeed + " PlayerSeed: " + playerSeed)

    val rules = Rules.StandardTwoPlayer

    Sim.run(
      rules = rules,
      gameSeed = gameSeed,
      playerSeed = playerSeed,
      players = Array(
        new RandomPlayer(playerSeed,0,rules),
        new RandomPlayer(playerSeed,1,rules)
      ),
      doPrint = true,
      useAnsiColors = true
    )
  }
}
