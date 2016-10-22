package fireflower

object Sim {
  def runSingle(
    rules: Rules,
    gameSeed: Long,
    playerSeed: Long,
    playerGen: PlayerGen,
    doPrint: Boolean,
    useAnsiColors: Boolean,
    debugTurnAndPath: Option[(Int,List[GiveAction])]
  ): Game = {
    val players = playerGen.genPlayers(rules,playerSeed)
    if(players.length != rules.numPlayers)
      throw new Exception("players.length (%d) != rules.numPlayers (%d)".format(players.length,rules.numPlayers))

    if(doPrint)
      println("GameSeed: " + gameSeed + " PlayerSeed: " + playerSeed)

    val game = Game(rules,gameSeed)
    game.drawInitialCards()

    for(pid <- 0 to (players.length - 1)) {
      players(pid).handleGameStart(game.hiddenFor(pid))
    }

    while(!game.isDone()) {
      debugTurnAndPath.foreach { case (turn,path) =>
        if(game.turnNumber == turn)
          game.debugPath = Some(path)
        else
          game.debugPath = None
      }

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
        for(pid <- 0 to (players.length - 1)) {
          players(pid).handleSeenAction(sa, game.hiddenFor(pid))
        }
      }
    }

    if(doPrint) {
      println(game.toString(useAnsiColors))
    }
    game
  }

  def runSingle(
    rules: Rules,
    playerGen: PlayerGen,
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
      playerGen = playerGen,
      doPrint = doPrint,
      useAnsiColors = useAnsiColors,
      debugTurnAndPath = None
    )
  }

  def runMulti(
    name: String,
    rules: Rules,
    numGames: Int,
    runSeed: Long,
    playerGen: PlayerGen,
    doPrint: Boolean,
    doPrintDetails: Boolean,
    useAnsiColors: Boolean
  ): List[Game] = {
    if(doPrint)
      println(name + " starting " + numGames + " games, runSeed: " + runSeed)

    val rand = Rand(runSeed)
    val games =
      (0 to (numGames-1)).map { i =>
        val gameSeed = rand.nextLong()
        val playerSeed = rand.nextLong()
        val game = runSingle(
          rules = rules,
          gameSeed = gameSeed,
          playerSeed = playerSeed,
          playerGen = playerGen,
          doPrint = doPrintDetails,
          useAnsiColors = useAnsiColors,
          debugTurnAndPath = None
        )
        if(doPrint)
          println(name + " Game " + i + " Score: " + game.numPlayed + " GameSeed: " + gameSeed)
        game
      }.toList
    games
  }

  def runMulti(
    name: String,
    rules: Rules,
    numGames: Int,
    playerGen: PlayerGen,
    doPrint: Boolean,
    doPrintDetails: Boolean,
    useAnsiColors: Boolean
  ): List[Game] = {
    val rand = Rand()
    runMulti(
      name = name,
      rules = rules,
      numGames = numGames,
      runSeed = rand.nextLong(),
      playerGen = playerGen,
      doPrint = doPrint,
      doPrintDetails = doPrintDetails,
      useAnsiColors = useAnsiColors
    )
  }
}
