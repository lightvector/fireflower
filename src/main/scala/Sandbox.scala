package fireflower

object Sandbox {

  def main(args: Array[String]): Unit = {
    //println("Hello world!")
    //RandTest.test()

    val debugTurnAndPath = {
      if(args.length >= 2) {
        val turn = args(1).toInt
        val pathstr = if(args.length >= 3) args(2) else ""
        val path = pathstr.split(",").filter(_.nonEmpty).map { s => GiveAction.ofString(s) }.toList
        Some((turn,path))
      }
      else
        None
    }

    val _game = Sim.runSingle(
      rules = Rules.Standard(numPlayers=2),
      gameSeed = args(0).toLong,
      playerSeed = 0L,
      playerGen = HeuristicPlayer,
      doPrint = true,
      useAnsiColors = true,
      debugTurnAndPath = debugTurnAndPath
    )

    // val _game = Sim.runSingle(
    //   rules = Rules.Standard(numPlayers=3),
    //   gameSeed = 0L,
    //   playerSeed = 0L,
    //   playerGen = HeuristicPlayer,
    //   doPrint = true,
    //   useAnsiColors = true,
    //   debugTurnAndPath = Some((16,List()))
    //   // debugTurnAndPath = Some((4,List(GiveHint(1,HintNumber(1)),GiveDiscard(3))))
    //   // debugTurnAndPath = Some((4,List(GiveDiscard(4),GiveDiscard(3))))
    // )

    //TODO bombs are not counted in the eval
    //TODO the play sequencing is bad
    //TODO often run out of hints and then can't protect 5s
    //TODO look at why this went wrong
    //-6310882139785350969
  }

}
