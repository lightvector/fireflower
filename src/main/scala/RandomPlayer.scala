package fireflower

  //RunSeed 0L
  // [info] Score  0  Games: 5356  Percent: 53.6%
  // [info] Score  1  Games: 2970  Percent: 29.7%
  // [info] Score  2  Games: 1176  Percent: 11.8%
  // [info] Score  3  Games: 369  Percent:  3.7%
  // [info] Score  4  Games: 91  Percent:  0.9%
  // [info] Score  5  Games: 28  Percent:  0.3%
  // [info] Score  6  Games:  7  Percent:  0.1%
  // [info] Score  7  Games:  3  Percent:  0.0%
  // [info] Score  8  Games:  0  Percent:  0.0%
  // [info] Score  9  Games:  0  Percent:  0.0%
  // [info] Score 10  Games:  0  Percent:  0.0%
  // [info] Score 11  Games:  0  Percent:  0.0%
  // [info] Score 12  Games:  0  Percent:  0.0%
  // [info] Score 13  Games:  0  Percent:  0.0%
  // [info] Score 14  Games:  0  Percent:  0.0%
  // [info] Score 15  Games:  0  Percent:  0.0%
  // [info] Score 16  Games:  0  Percent:  0.0%
  // [info] Score 17  Games:  0  Percent:  0.0%
  // [info] Score 18  Games:  0  Percent:  0.0%
  // [info] Score 19  Games:  0  Percent:  0.0%
  // [info] Score 20  Games:  0  Percent:  0.0%
  // [info] Score 21  Games:  0  Percent:  0.0%
  // [info] Score 22  Games:  0  Percent:  0.0%
  // [info] Score 23  Games:  0  Percent:  0.0%
  // [info] Score 24  Games:  0  Percent:  0.0%
  // [info] Score 25  Games:  0  Percent:  0.0%
  // [info] Average Utility: 1.3992


class RandomPlayer(val seed: Long, val myPid: Int, val rules: Rules) extends Player {
  val rand = Rand(Array(seed,myPid.toLong))
  val possibleHintTypes: Array[GiveHintType] = rules.possibleHintTypes()

  override def handleGameStart(game: Game): Unit = {}
  override def handleSeenAction(preGame: Game, sa: SeenAction, postGame: Game): Unit = {}

  override def getAction(game: Game): GiveAction = {
    rand.nextInt(5) match {
      case 0 =>
        GivePlay(rand.nextInt(game.hands(myPid).numCards))
      case 1 | 2 =>
        if(game.numHints < rules.maxHints)
          GiveDiscard(rand.nextInt(game.hands(myPid).numCards))
        else
          getAction(game)
      case 3 | 4 =>
        if(game.numHints <= 0)
          getAction(game)
        else {
          var hintPid = rand.nextInt(rules.numPlayers-1)
          if(hintPid >= myPid)
            hintPid += 1
          var hintType = possibleHintTypes(rand.nextInt(possibleHintTypes.length))
          while(!game.hands(hintPid).exists { cid => rules.hintApplies(hintType, game.seenMap(cid)) })
            hintType = possibleHintTypes(rand.nextInt(possibleHintTypes.length))
          GiveHint(hintPid, hintType)
        }
    }
  }
}
