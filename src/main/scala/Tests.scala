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
   [info] Score  2  Games: 18  Percent:  1.8%  Cum: 99.3%
   [info] Score  3  Games: 23  Percent:  2.3%  Cum: 97.5%
   [info] Score  4  Games: 27  Percent:  2.7%  Cum: 95.2%
   [info] Score  5  Games: 35  Percent:  3.5%  Cum: 92.5%
   [info] Score  6  Games: 33  Percent:  3.3%  Cum: 89.0%
   [info] Score  7  Games: 30  Percent:  3.0%  Cum: 85.7%
   [info] Score  8  Games: 25  Percent:  2.5%  Cum: 82.7%
   [info] Score  9  Games: 34  Percent:  3.4%  Cum: 80.2%
   [info] Score 10  Games: 35  Percent:  3.5%  Cum: 76.8%
   [info] Score 11  Games: 34  Percent:  3.4%  Cum: 73.3%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 69.9%
   [info] Score 13  Games: 26  Percent:  2.6%  Cum: 67.2%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 64.6%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 62.1%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 59.9%
   [info] Score 17  Games: 30  Percent:  3.0%  Cum: 57.4%
   [info] Score 18  Games: 31  Percent:  3.1%  Cum: 54.4%
   [info] Score 19  Games: 22  Percent:  2.2%  Cum: 51.3%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 49.1%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 46.8%
   [info] Score 22  Games: 34  Percent:  3.4%  Cum: 44.2%
   [info] Score 23  Games: 53  Percent:  5.3%  Cum: 40.8%
   [info] Score 24  Games: 62  Percent:  6.2%  Cum: 35.5%
   [info] Score 25  Games: 293  Percent: 29.3%  Cum: 29.3%
   [info] Average Score: 17.045
   [info] Average Utility: 48.74
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.6%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 97.4%
   [info] Score  5  Games: 15  Percent:  1.5%  Cum: 96.2%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 94.7%
   [info] Score  7  Games: 18  Percent:  1.8%  Cum: 93.1%
   [info] Score  8  Games: 13  Percent:  1.3%  Cum: 91.3%
   [info] Score  9  Games: 28  Percent:  2.8%  Cum: 90.0%
   [info] Score 10  Games: 23  Percent:  2.3%  Cum: 87.2%
   [info] Score 11  Games: 24  Percent:  2.4%  Cum: 84.9%
   [info] Score 12  Games: 21  Percent:  2.1%  Cum: 82.5%
   [info] Score 13  Games: 20  Percent:  2.0%  Cum: 80.4%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 78.4%
   [info] Score 15  Games: 32  Percent:  3.2%  Cum: 75.3%
   [info] Score 16  Games: 41  Percent:  4.1%  Cum: 72.1%
   [info] Score 17  Games: 36  Percent:  3.6%  Cum: 68.0%
   [info] Score 18  Games: 45  Percent:  4.5%  Cum: 64.4%
   [info] Score 19  Games: 43  Percent:  4.3%  Cum: 59.9%
   [info] Score 20  Games: 38  Percent:  3.8%  Cum: 55.6%
   [info] Score 21  Games: 44  Percent:  4.4%  Cum: 51.8%
   [info] Score 22  Games: 62  Percent:  6.2%  Cum: 47.4%
   [info] Score 23  Games: 87  Percent:  8.7%  Cum: 41.2%
   [info] Score 24  Games: 200  Percent: 20.0%  Cum: 32.5%
   [info] Score 25  Games: 125  Percent: 12.5%  Cum: 12.5%
   [info] Average Score: 18.543
   [info] Average Utility: 43.336
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.2%
   [info] Score  3  Games:  6  Percent:  0.6%  Cum: 98.3%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 97.7%
   [info] Score  5  Games: 17  Percent:  1.7%  Cum: 96.9%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 95.2%
   [info] Score  7  Games: 21  Percent:  2.1%  Cum: 94.1%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 92.0%
   [info] Score  9  Games: 22  Percent:  2.2%  Cum: 90.3%
   [info] Score 10  Games: 21  Percent:  2.1%  Cum: 88.1%
   [info] Score 11  Games: 36  Percent:  3.6%  Cum: 86.0%
   [info] Score 12  Games: 28  Percent:  2.8%  Cum: 82.4%
   [info] Score 13  Games: 40  Percent:  4.0%  Cum: 79.6%
   [info] Score 14  Games: 42  Percent:  4.2%  Cum: 75.6%
   [info] Score 15  Games: 46  Percent:  4.6%  Cum: 71.4%
   [info] Score 16  Games: 45  Percent:  4.5%  Cum: 66.8%
   [info] Score 17  Games: 60  Percent:  6.0%  Cum: 62.3%
   [info] Score 18  Games: 53  Percent:  5.3%  Cum: 56.3%
   [info] Score 19  Games: 61  Percent:  6.1%  Cum: 51.0%
   [info] Score 20  Games: 55  Percent:  5.5%  Cum: 44.9%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 39.4%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 34.8%
   [info] Score 23  Games: 121  Percent: 12.1%  Cum: 28.1%
   [info] Score 24  Games: 131  Percent: 13.1%  Cum: 16.0%
   [info] Score 25  Games: 29  Percent:  2.9%  Cum:  2.9%
   [info] Average Score: 17.49
   [info] Average Utility: 36.43
   [info]
   [info] Time: 1035.64532162

   */

}
