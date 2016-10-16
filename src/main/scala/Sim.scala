package fireflower

object Sim {
  def run(
    rules: Rules,
    gameSeed: Long,
    playerSeed: Long,
    players: Array[Player],
    doPrint: Boolean,
    useAnsiColors: Boolean
  ): Unit = {
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

    if(doPrint)
      println(game.toString(useAnsiColors))
  }
}
