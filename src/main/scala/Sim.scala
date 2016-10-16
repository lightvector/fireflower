package fireflower

object Sim {
  def runSingle(
    rules: Rules,
    gameSeed: Long,
    playerSeed: Long,
    players: Array[Player],
    doPrint: Boolean,
    useAnsiColors: Boolean
  ): Game = {
    if(doPrint)
      println("GameSeed: " + gameSeed + " PlayerSeed: " + playerSeed)

    val game = Game(rules,gameSeed)
    game.drawInitialCards()

    for(pid <- 0 to (players.length - 1)) {
      players(pid).handleGameStart(game.hiddenFor(pid))
    }

    while(!game.isDone()) {
      val player = players(game.curPlayer)
      val ga = player.getAction(game.hiddenFor(game.curPlayer))
      if(!game.isLegal(ga)) {
        throw new Exception("Illegal action: " + game.giveActionToString(ga))
      }
      else {
        val sa = game.seenAction(ga)
        if(doPrint)
          println(game.toString(useAnsiColors) + "  " + game.seenActionToString(sa,useAnsiColors))
        game.doAction(ga)
      }
    }

    if(doPrint) {
      println(game.toString(useAnsiColors))
      println("Score: " + game.numPlayed)
    }
    game
  }

  def runSingle(
    rules: Rules,
    players: Long => Array[Player],
    doPrint: Boolean,
    useAnsiColors: Boolean
  ): Game = {
    val rand = Rand()
    val gameSeed = rand.nextLong()
    val playerSeed = rand.nextLong()
    runSingle(
      rules = rules,
      gameSeed = gameSeed,
      playerSeed = playerSeed,
      players = players(playerSeed),
      doPrint = doPrint,
      useAnsiColors = useAnsiColors
    )
  }

  def runMulti(
    rules: Rules,
    reps: Int,
    runSeed: Long,
    players: Long => Array[Player],
    doPrint: Boolean,
    doPrintDetails: Boolean,
    useAnsiColors: Boolean
  ): List[Game] = {
    val rand = Rand(runSeed)
    val games =
      (1 to reps).map { _ =>
        val gameSeed = rand.nextLong()
        val playerSeed = rand.nextLong()
        runSingle(
          rules = rules,
          gameSeed = gameSeed,
          playerSeed = playerSeed,
          players = players(playerSeed),
          doPrint = doPrintDetails,
          useAnsiColors = useAnsiColors
        )
      }.toList

    if(doPrint) {
      val scoreTable = (0 to rules.maxScore).map { score =>
        (score,games.count(game => game.numPlayed == score))
      }
      scoreTable.foreach { case (score,count) =>
        println("Score %2d  Games: %2d  Percent: %4.1f%%".format(score, count, count.toDouble * 100.0 / reps))
      }
      val avgUtility = scoreTable.foldLeft(0) { case (acc,(score,count)) =>
        acc + count * (if(score == rules.maxScore) score * 4 else score * 2)
      }.toDouble / reps
      println("Average Utility: " + avgUtility)
    }

    games
  }

  def runMulti(
    rules: Rules,
    reps: Int,
    players: Long => Array[Player],
    doPrint: Boolean,
    doPrintDetails: Boolean,
    useAnsiColors: Boolean
  ): List[Game] = {
    val rand = Rand()
    runMulti(
      rules = rules,
      reps = reps,
      runSeed = rand.nextLong(),
      players = players,
      doPrint = doPrint,
      doPrintDetails = doPrintDetails,
      useAnsiColors = useAnsiColors
    )
  }
}
