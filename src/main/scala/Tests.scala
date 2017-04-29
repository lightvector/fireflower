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

    runTests(prefix="",salt="d",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 100.0%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.6%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.9%
   [info] Score  4  Games: 11  Percent:  1.1%  Cum: 97.9%
   [info] Score  5  Games: 23  Percent:  2.3%  Cum: 96.8%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 94.5%
   [info] Score  7  Games: 16  Percent:  1.6%  Cum: 93.1%
   [info] Score  8  Games: 22  Percent:  2.2%  Cum: 91.5%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 89.3%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 87.9%
   [info] Score 11  Games: 16  Percent:  1.6%  Cum: 86.1%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 84.5%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 82.6%
   [info] Score 14  Games: 21  Percent:  2.1%  Cum: 80.5%
   [info] Score 15  Games: 13  Percent:  1.3%  Cum: 78.4%
   [info] Score 16  Games: 28  Percent:  2.8%  Cum: 77.1%
   [info] Score 17  Games: 16  Percent:  1.6%  Cum: 74.3%
   [info] Score 18  Games: 23  Percent:  2.3%  Cum: 72.7%
   [info] Score 19  Games: 20  Percent:  2.0%  Cum: 70.4%
   [info] Score 20  Games: 18  Percent:  1.8%  Cum: 68.4%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 66.6%
   [info] Score 22  Games: 33  Percent:  3.3%  Cum: 63.9%
   [info] Score 23  Games: 38  Percent:  3.8%  Cum: 60.6%
   [info] Score 24  Games: 75  Percent:  7.5%  Cum: 56.8%
   [info] Score 25  Games: 493  Percent: 49.3%  Cum: 49.3%
   [info] Average Score: 20.217
   [info] Average Utility: 65.084
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  4  Games:  4  Percent:  0.4%  Cum: 99.7%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 99.3%
   [info] Score  6  Games: 13  Percent:  1.3%  Cum: 98.6%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 97.3%
   [info] Score  8  Games: 11  Percent:  1.1%  Cum: 96.3%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 95.2%
   [info] Score 10  Games: 12  Percent:  1.2%  Cum: 93.6%
   [info] Score 11  Games: 12  Percent:  1.2%  Cum: 92.4%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 91.2%
   [info] Score 13  Games:  9  Percent:  0.9%  Cum: 88.7%
   [info] Score 14  Games: 24  Percent:  2.4%  Cum: 87.8%
   [info] Score 15  Games: 17  Percent:  1.7%  Cum: 85.4%
   [info] Score 16  Games: 28  Percent:  2.8%  Cum: 83.7%
   [info] Score 17  Games: 18  Percent:  1.8%  Cum: 80.9%
   [info] Score 18  Games: 21  Percent:  2.1%  Cum: 79.1%
   [info] Score 19  Games: 32  Percent:  3.2%  Cum: 77.0%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 73.8%
   [info] Score 21  Games: 28  Percent:  2.8%  Cum: 70.3%
   [info] Score 22  Games: 65  Percent:  6.5%  Cum: 67.5%
   [info] Score 23  Games: 83  Percent:  8.3%  Cum: 61.0%
   [info] Score 24  Games: 174  Percent: 17.4%  Cum: 52.7%
   [info] Score 25  Games: 353  Percent: 35.3%  Cum: 35.3%
   [info] Average Score: 21.066
   [info] Average Utility: 59.782
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.6%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.6%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 99.1%
   [info] Score  6  Games:  6  Percent:  0.6%  Cum: 98.5%
   [info] Score  7  Games: 12  Percent:  1.2%  Cum: 97.9%
   [info] Score  8  Games: 11  Percent:  1.1%  Cum: 96.7%
   [info] Score  9  Games: 13  Percent:  1.3%  Cum: 95.6%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 94.3%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 92.6%
   [info] Score 12  Games: 17  Percent:  1.7%  Cum: 90.4%
   [info] Score 13  Games: 19  Percent:  1.9%  Cum: 88.7%
   [info] Score 14  Games: 20  Percent:  2.0%  Cum: 86.8%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 84.8%
   [info] Score 16  Games: 36  Percent:  3.6%  Cum: 82.5%
   [info] Score 17  Games: 32  Percent:  3.2%  Cum: 78.9%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 75.7%
   [info] Score 19  Games: 49  Percent:  4.9%  Cum: 72.4%
   [info] Score 20  Games: 52  Percent:  5.2%  Cum: 67.5%
   [info] Score 21  Games: 55  Percent:  5.5%  Cum: 62.3%
   [info] Score 22  Games: 101  Percent: 10.1%  Cum: 56.8%
   [info] Score 23  Games: 115  Percent: 11.5%  Cum: 46.7%
   [info] Score 24  Games: 202  Percent: 20.2%  Cum: 35.2%
   [info] Score 25  Games: 150  Percent: 15.0%  Cum: 15.0%
   [info] Average Score: 20.173
   [info] Average Utility: 47.846
   [info]
   [info] Time: 1589.564550855

   */

}
