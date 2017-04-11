package fireflower

object PlayerTests {

  def main(args: Array[String]): Unit = {
    val numGames = {
      if(args.length >= 1)
        args(0).toInt
      else
        1000
    }
    val numPlayers = {
      if(args.length >= 2)
        Some(args(1).toInt)
      else
        None
    }
    println("NumGames=" + numGames)

    runTests(prefix="",salt="a",numGames=numGames, numPlayers=numPlayers)
  }

  def runTests(prefix: String, salt: String, numGames: Int, numPlayers: Option[Int]): Unit = {
    val start = System.nanoTime()
    val rules2p = Rules.Standard(numPlayers=2)
    val rules3p = Rules.Standard(numPlayers=3)
    val rules4p = Rules.Standard(numPlayers=4)

    def makeRunSeed(name:String): Long = {
      RandUtils.sha256Long(RandUtils.sha256Long(name) + salt)
    }

    val name2p = prefix + "HeuristicStandard2P"
    val games2p = {
      Sim.runMulti(
        name = name2p,
        rules = rules2p,
        numGames,
        runSeed = makeRunSeed(name2p),
        playerGen = HeuristicPlayer,
        doPrint = true,
        doPrintDetails = false,
        useAnsiColors = true
      )
    }
    println(name2p + ":")
    printScoreSummary(rules2p,games2p)

    val name3p = prefix + "HeuristicStandard3P"
    val games3p = {
      Sim.runMulti(
        name = name3p,
        rules = rules3p,
        numGames,
        runSeed = makeRunSeed(name3p),
        playerGen = HeuristicPlayer,
        doPrint = true,
        doPrintDetails = false,
        useAnsiColors = true
      )
    }
    println(name3p + ":")
    printScoreSummary(rules3p,games3p)

    val name4p = prefix + "HeuristicStandard4P"
    val games4p = {
      Sim.runMulti(
        name = name4p,
        rules = rules4p,
        numGames,
        runSeed = makeRunSeed(name4p),
        playerGen = HeuristicPlayer,
        doPrint = true,
        doPrintDetails = false,
        useAnsiColors = true
      )
    }
    println(name4p + ":")
    printScoreSummary(rules4p,games4p)

    val end = System.nanoTime()

    println("Done!")
    println("")
    println(name2p + ":")
    printScoreSummary(rules2p,games2p)
    println("")
    println(name3p + ":")
    printScoreSummary(rules3p,games3p)
    println("")
    println(name4p + ":")
    printScoreSummary(rules4p,games4p)
    println("")
    println("Time: " + (end-start).toDouble / 1.0e9)
  }

  def printScoreSummary(rules: Rules, games: List[Game]) = {
    val scoreTable = (0 to rules.maxScore).map { score =>
      (score,games.count(game => game.numPlayed == score))
    }
    val numGames = games.length
    var cumulativeCount = 0
    scoreTable.foreach { case (score,count) =>
      println("Score %2d  Games: %2d  Percent: %4.1f%%  Cum: %4.1f%%".format(
        score, count, count.toDouble * 100.0 / numGames, (numGames - cumulativeCount.toDouble) * 100.0 / numGames
      ))
      cumulativeCount += count
    }
    val avgScore = scoreTable.foldLeft(0) { case (acc,(score,count)) =>
      acc + count * score
    }.toDouble / numGames
    val avgUtility = scoreTable.foldLeft(0) { case (acc,(score,count)) =>
      acc + count * (if(score == rules.maxScore) score * 4 else score * 2)
    }.toDouble / numGames
    println("Average Score: " + avgScore)
    println("Average Utility: " + avgUtility)
  }

  /*
   Results:

   [info] HeuristicStandard2P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  2  Games: 15  Percent:  1.5%  Cum: 99.3%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 97.8%
   [info] Score  4  Games: 25  Percent:  2.5%  Cum: 96.2%
   [info] Score  5  Games: 28  Percent:  2.8%  Cum: 93.7%
   [info] Score  6  Games: 26  Percent:  2.6%  Cum: 90.9%
   [info] Score  7  Games: 27  Percent:  2.7%  Cum: 88.3%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 85.6%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 82.8%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 80.4%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 78.4%
   [info] Score 12  Games: 33  Percent:  3.3%  Cum: 75.7%
   [info] Score 13  Games: 17  Percent:  1.7%  Cum: 72.4%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 70.7%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 67.6%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 65.2%
   [info] Score 17  Games: 32  Percent:  3.2%  Cum: 62.7%
   [info] Score 18  Games: 30  Percent:  3.0%  Cum: 59.5%
   [info] Score 19  Games: 24  Percent:  2.4%  Cum: 56.5%
   [info] Score 20  Games: 20  Percent:  2.0%  Cum: 54.1%
   [info] Score 21  Games: 35  Percent:  3.5%  Cum: 52.1%
   [info] Score 22  Games: 18  Percent:  1.8%  Cum: 48.6%
   [info] Score 23  Games: 46  Percent:  4.6%  Cum: 46.8%
   [info] Score 24  Games: 75  Percent:  7.5%  Cum: 42.2%
   [info] Score 25  Games: 347  Percent: 34.7%  Cum: 34.7%
   [info] Average Score: 18.02
   [info] Average Utility: 53.39
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games: 10  Percent:  1.0%  Cum: 99.9%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 98.9%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.4%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 97.4%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 96.2%
   [info] Score  6  Games: 21  Percent:  2.1%  Cum: 94.6%
   [info] Score  7  Games: 17  Percent:  1.7%  Cum: 92.5%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 90.8%
   [info] Score  9  Games: 17  Percent:  1.7%  Cum: 89.1%
   [info] Score 10  Games: 26  Percent:  2.6%  Cum: 87.4%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 84.8%
   [info] Score 12  Games: 13  Percent:  1.3%  Cum: 82.5%
   [info] Score 13  Games: 20  Percent:  2.0%  Cum: 81.2%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 79.2%
   [info] Score 15  Games: 28  Percent:  2.8%  Cum: 77.0%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 74.2%
   [info] Score 17  Games: 30  Percent:  3.0%  Cum: 71.7%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 68.7%
   [info] Score 19  Games: 39  Percent:  3.9%  Cum: 65.4%
   [info] Score 20  Games: 34  Percent:  3.4%  Cum: 61.5%
   [info] Score 21  Games: 30  Percent:  3.0%  Cum: 58.1%
   [info] Score 22  Games: 66  Percent:  6.6%  Cum: 55.1%
   [info] Score 23  Games: 74  Percent:  7.4%  Cum: 48.5%
   [info] Score 24  Games: 219  Percent: 21.9%  Cum: 41.1%
   [info] Score 25  Games: 192  Percent: 19.2%  Cum: 19.2%
   [info] Average Score: 19.134
   [info] Average Utility: 47.868
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.3%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 98.4%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 97.6%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 96.9%
   [info] Score  6  Games:  9  Percent:  0.9%  Cum: 94.9%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 94.0%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 91.7%
   [info] Score  9  Games: 19  Percent:  1.9%  Cum: 90.2%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 88.3%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 86.4%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 84.1%
   [info] Score 13  Games: 40  Percent:  4.0%  Cum: 81.4%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 77.4%
   [info] Score 15  Games: 37  Percent:  3.7%  Cum: 74.1%
   [info] Score 16  Games: 34  Percent:  3.4%  Cum: 70.4%
   [info] Score 17  Games: 45  Percent:  4.5%  Cum: 67.0%
   [info] Score 18  Games: 54  Percent:  5.4%  Cum: 62.5%
   [info] Score 19  Games: 53  Percent:  5.3%  Cum: 57.1%
   [info] Score 20  Games: 52  Percent:  5.2%  Cum: 51.8%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 46.6%
   [info] Score 22  Games: 86  Percent:  8.6%  Cum: 42.0%
   [info] Score 23  Games: 103  Percent: 10.3%  Cum: 33.4%
   [info] Score 24  Games: 164  Percent: 16.4%  Cum: 23.1%
   [info] Score 25  Games: 67  Percent:  6.7%  Cum:  6.7%
   [info] Average Score: 18.151
   [info] Average Utility: 39.652
   [info]
   [info] Time: 982.451022654

   */

}
