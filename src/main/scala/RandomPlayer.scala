package fireflower

class RandomPlayer(val seed: Long, val pid: Int, val rules: Rules) extends Player {
  val rand = Rand(Array(seed,pid.toLong))
  val possibleHintTypes: Array[GiveHintType] = rules.possibleHintTypes()

  def handleGameStart(game: Game): Unit = {

  }
  def getAction(game: Game): GiveAction = {
    rand.nextInt(5) match {
      case 0 =>
        GivePlay(rand.nextInt(game.hands(pid).numCards))
      case 1 | 2 =>
        if(game.numHints < rules.maxHints)
          GiveDiscard(rand.nextInt(game.hands(pid).numCards))
        else
          getAction(game)
      case 3 | 4 =>
        var hintPid = rand.nextInt(rules.numPlayers-1)
        if(hintPid >= pid)
          hintPid += 1
        var hintType = possibleHintTypes(rand.nextInt(possibleHintTypes.length))
        while(!game.hands(hintPid).exists { cid => rules.hintApplies(hintType, game.cardMap(cid)) })
          hintType = possibleHintTypes(rand.nextInt(possibleHintTypes.length))
        GiveHint(hintPid, hintType)
    }
  }
}
