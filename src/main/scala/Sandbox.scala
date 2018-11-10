package fireflower

object Sandbox {

  //Usage: <SEED> (<DEBUGTURN>) (<DEBUG ACTION SEQUENCE>)
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
      rules = Rules.Standard(numPlayers=2,stopEarlyLoss=true),
      // rules = Rules.Standard(numPlayers=3),
      gameSeed = args(0).toLong,
      playerSeed = 0L,
      playerGen = HeuristicPlayer,
      doPrint = true,
      useAnsiColors = true,
      debugTurnAndPath = debugTurnAndPath
    )
  }

}
