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

    runTests(prefix="",salt="b",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.9%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 99.5%
   [info] Score  4  Games: 20  Percent:  2.0%  Cum: 98.7%
   [info] Score  5  Games: 17  Percent:  1.7%  Cum: 96.7%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 95.0%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 93.2%
   [info] Score  8  Games: 24  Percent:  2.4%  Cum: 91.0%
   [info] Score  9  Games: 23  Percent:  2.3%  Cum: 88.6%
   [info] Score 10  Games: 28  Percent:  2.8%  Cum: 86.3%
   [info] Score 11  Games: 34  Percent:  3.4%  Cum: 83.5%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 80.1%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 77.6%
   [info] Score 14  Games: 19  Percent:  1.9%  Cum: 74.6%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 72.7%
   [info] Score 16  Games: 18  Percent:  1.8%  Cum: 70.3%
   [info] Score 17  Games: 22  Percent:  2.2%  Cum: 68.5%
   [info] Score 18  Games: 31  Percent:  3.1%  Cum: 66.3%
   [info] Score 19  Games: 28  Percent:  2.8%  Cum: 63.2%
   [info] Score 20  Games: 24  Percent:  2.4%  Cum: 60.4%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 58.0%
   [info] Score 22  Games: 27  Percent:  2.7%  Cum: 55.3%
   [info] Score 23  Games: 35  Percent:  3.5%  Cum: 52.6%
   [info] Score 24  Games: 79  Percent:  7.9%  Cum: 49.1%
   [info] Score 25  Games: 412  Percent: 41.2%  Cum: 41.2%
   [info] Average Score: 19.223
   [info] Average Utility: 59.046
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 98.8%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.2%
   [info] Score  7  Games:  7  Percent:  0.7%  Cum: 97.7%
   [info] Score  8  Games:  8  Percent:  0.8%  Cum: 97.0%
   [info] Score  9  Games:  8  Percent:  0.8%  Cum: 96.2%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 95.4%
   [info] Score 11  Games: 17  Percent:  1.7%  Cum: 93.5%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 91.8%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 89.4%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 87.1%
   [info] Score 15  Games: 17  Percent:  1.7%  Cum: 84.6%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 82.9%
   [info] Score 17  Games: 31  Percent:  3.1%  Cum: 80.2%
   [info] Score 18  Games: 32  Percent:  3.2%  Cum: 77.1%
   [info] Score 19  Games: 37  Percent:  3.7%  Cum: 73.9%
   [info] Score 20  Games: 36  Percent:  3.6%  Cum: 70.2%
   [info] Score 21  Games: 41  Percent:  4.1%  Cum: 66.6%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 62.5%
   [info] Score 23  Games: 73  Percent:  7.3%  Cum: 55.8%
   [info] Score 24  Games: 212  Percent: 21.2%  Cum: 48.5%
   [info] Score 25  Games: 273  Percent: 27.3%  Cum: 27.3%
   [info] Average Score: 20.74
   [info] Average Utility: 55.13
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.8%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  4  Games:  4  Percent:  0.4%  Cum: 99.4%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 99.0%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.2%
   [info] Score  7  Games: 11  Percent:  1.1%  Cum: 97.8%
   [info] Score  8  Games: 13  Percent:  1.3%  Cum: 96.7%
   [info] Score  9  Games: 10  Percent:  1.0%  Cum: 95.4%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 94.4%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 92.6%
   [info] Score 12  Games: 26  Percent:  2.6%  Cum: 90.3%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 87.7%
   [info] Score 14  Games: 29  Percent:  2.9%  Cum: 85.4%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 82.5%
   [info] Score 16  Games: 40  Percent:  4.0%  Cum: 79.6%
   [info] Score 17  Games: 42  Percent:  4.2%  Cum: 75.6%
   [info] Score 18  Games: 53  Percent:  5.3%  Cum: 71.4%
   [info] Score 19  Games: 51  Percent:  5.1%  Cum: 66.1%
   [info] Score 20  Games: 50  Percent:  5.0%  Cum: 61.0%
   [info] Score 21  Games: 70  Percent:  7.0%  Cum: 56.0%
   [info] Score 22  Games: 92  Percent:  9.2%  Cum: 49.0%
   [info] Score 23  Games: 143  Percent: 14.3%  Cum: 39.8%
   [info] Score 24  Games: 173  Percent: 17.3%  Cum: 25.5%
   [info] Score 25  Games: 82  Percent:  8.2%  Cum:  8.2%
   [info] Average Score: 19.511
   [info] Average Utility: 43.122
   [info]
   [info] Time: 1128.523120844

   */

}
