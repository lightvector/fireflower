package fireflower

object Sandbox {

  def main(args: Array[String]): Unit = {
    //println("Hello world!")
    //RandTest.test()

    val _game = Sim.runSingle(
      rules = Rules.Standard(numPlayers=2),
      gameSeed = 4956284275229322651L,
      playerSeed = 7246858482769613123L,
      playerGen = HeuristicPlayer,
      doPrint = true,
      useAnsiColors = true,
      // debugTurnAndPath = Some((16,List(GiveHint(1,HintColor(Green)),GiveDiscard(2))))
      // debugTurnAndPath = Some((16,List(GiveHint(1,HintNumber(4)),GiveDiscard(2))))
      // debugTurnAndPath = Some((4,List(GiveHint(1,HintNumber(1)),GiveDiscard(3))))
      // debugTurnAndPath = Some((26,List()))
      // debugTurnAndPath = Some((26,List(GivePlay(4),GiveDiscard(0))))
      debugTurnAndPath = Some((26,List(GiveHint(1,HintNumber(0)),GivePlay(0))))
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
