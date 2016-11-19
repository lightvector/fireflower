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

   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.8%
   [info] Score  2  Games: 11  Percent:  1.1%  Cum: 99.2%
   [info] Score  3  Games: 15  Percent:  1.5%  Cum: 98.1%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 96.6%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 95.4%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 93.4%
   [info] Score  7  Games: 16  Percent:  1.6%  Cum: 92.3%
   [info] Score  8  Games: 19  Percent:  1.9%  Cum: 90.7%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 88.8%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 86.4%
   [info] Score 11  Games: 33  Percent:  3.3%  Cum: 84.6%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 81.3%
   [info] Score 13  Games: 41  Percent:  4.1%  Cum: 78.8%
   [info] Score 14  Games: 38  Percent:  3.8%  Cum: 74.7%
   [info] Score 15  Games: 40  Percent:  4.0%  Cum: 70.9%
   [info] Score 16  Games: 40  Percent:  4.0%  Cum: 66.9%
   [info] Score 17  Games: 41  Percent:  4.1%  Cum: 62.9%
   [info] Score 18  Games: 53  Percent:  5.3%  Cum: 58.8%
   [info] Score 19  Games: 63  Percent:  6.3%  Cum: 53.5%
   [info] Score 20  Games: 53  Percent:  5.3%  Cum: 47.2%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 41.9%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 37.3%
   [info] Score 23  Games: 124  Percent: 12.4%  Cum: 30.6%
   [info] Score 24  Games: 154  Percent: 15.4%  Cum: 18.2%
   [info] Score 25  Games: 28  Percent:  2.8%  Cum:  2.8%
   [info] Average Score: 17.511
   [info] Average Utility: 36.422
   [info] Done!
   [info]
   [info] HeuristicStandard2P:
   [info] Score  0  Games:  4  Percent:  0.4%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  2  Games: 15  Percent:  1.5%  Cum: 99.0%
   [info] Score  3  Games: 28  Percent:  2.8%  Cum: 97.5%
   [info] Score  4  Games: 33  Percent:  3.3%  Cum: 94.7%
   [info] Score  5  Games: 38  Percent:  3.8%  Cum: 91.4%
   [info] Score  6  Games: 35  Percent:  3.5%  Cum: 87.6%
   [info] Score  7  Games: 44  Percent:  4.4%  Cum: 84.1%
   [info] Score  8  Games: 32  Percent:  3.2%  Cum: 79.7%
   [info] Score  9  Games: 29  Percent:  2.9%  Cum: 76.5%
   [info] Score 10  Games: 36  Percent:  3.6%  Cum: 73.6%
   [info] Score 11  Games: 31  Percent:  3.1%  Cum: 70.0%
   [info] Score 12  Games: 38  Percent:  3.8%  Cum: 66.9%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 63.1%
   [info] Score 14  Games: 35  Percent:  3.5%  Cum: 60.1%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 56.6%
   [info] Score 16  Games: 22  Percent:  2.2%  Cum: 54.3%
   [info] Score 17  Games: 34  Percent:  3.4%  Cum: 52.1%
   [info] Score 18  Games: 18  Percent:  1.8%  Cum: 48.7%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 46.9%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 44.8%
   [info] Score 21  Games: 41  Percent:  4.1%  Cum: 42.5%
   [info] Score 22  Games: 30  Percent:  3.0%  Cum: 38.4%
   [info] Score 23  Games: 23  Percent:  2.3%  Cum: 35.4%
   [info] Score 24  Games: 54  Percent:  5.4%  Cum: 33.1%
   [info] Score 25  Games: 277  Percent: 27.7%  Cum: 27.7%
   [info] Average Score: 16.243
   [info] Average Utility: 46.336
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games: 11  Percent:  1.1%  Cum: 99.8%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 98.7%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.2%
   [info] Score  4  Games: 15  Percent:  1.5%  Cum: 96.6%
   [info] Score  5  Games: 14  Percent:  1.4%  Cum: 95.1%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 93.7%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 92.0%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 89.7%
   [info] Score  9  Games: 19  Percent:  1.9%  Cum: 87.9%
   [info] Score 10  Games: 32  Percent:  3.2%  Cum: 86.0%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 82.8%
   [info] Score 12  Games: 31  Percent:  3.1%  Cum: 80.6%
   [info] Score 13  Games: 32  Percent:  3.2%  Cum: 77.5%
   [info] Score 14  Games: 36  Percent:  3.6%  Cum: 74.3%
   [info] Score 15  Games: 25  Percent:  2.5%  Cum: 70.7%
   [info] Score 16  Games: 34  Percent:  3.4%  Cum: 68.2%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 64.8%
   [info] Score 18  Games: 43  Percent:  4.3%  Cum: 62.3%
   [info] Score 19  Games: 33  Percent:  3.3%  Cum: 58.0%
   [info] Score 20  Games: 40  Percent:  4.0%  Cum: 54.7%
   [info] Score 21  Games: 43  Percent:  4.3%  Cum: 50.7%
   [info] Score 22  Games: 46  Percent:  4.6%  Cum: 46.4%
   [info] Score 23  Games: 75  Percent:  7.5%  Cum: 41.8%
   [info] Score 24  Games: 218  Percent: 21.8%  Cum: 34.3%
   [info] Score 25  Games: 125  Percent: 12.5%  Cum: 12.5%
   [info] Average Score: 18.173
   [info] Average Utility: 42.596
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.8%
   [info] Score  2  Games: 11  Percent:  1.1%  Cum: 99.2%
   [info] Score  3  Games: 15  Percent:  1.5%  Cum: 98.1%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 96.6%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 95.4%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 93.4%
   [info] Score  7  Games: 16  Percent:  1.6%  Cum: 92.3%
   [info] Score  8  Games: 19  Percent:  1.9%  Cum: 90.7%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 88.8%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 86.4%
   [info] Score 11  Games: 33  Percent:  3.3%  Cum: 84.6%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 81.3%
   [info] Score 13  Games: 41  Percent:  4.1%  Cum: 78.8%
   [info] Score 14  Games: 38  Percent:  3.8%  Cum: 74.7%
   [info] Score 15  Games: 40  Percent:  4.0%  Cum: 70.9%
   [info] Score 16  Games: 40  Percent:  4.0%  Cum: 66.9%
   [info] Score 17  Games: 41  Percent:  4.1%  Cum: 62.9%
   [info] Score 18  Games: 53  Percent:  5.3%  Cum: 58.8%
   [info] Score 19  Games: 63  Percent:  6.3%  Cum: 53.5%
   [info] Score 20  Games: 53  Percent:  5.3%  Cum: 47.2%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 41.9%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 37.3%
   [info] Score 23  Games: 124  Percent: 12.4%  Cum: 30.6%
   [info] Score 24  Games: 154  Percent: 15.4%  Cum: 18.2%
   [info] Score 25  Games: 28  Percent:  2.8%  Cum:  2.8%
   [info] Average Score: 17.511
   [info] Average Utility: 36.422
   [info]
   [info] Time: 944.800556669

   */

}
