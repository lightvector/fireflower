package fireflower

object PlayerTests {

  def main(args: Array[String]): Unit = {
    runTests(prefix="",numGames=1000)
    // runTests(prefix="Quick2",numGames=100)
  }

  def runTests(prefix:String, numGames: Int): Unit = {
    val start = System.nanoTime()
    val rules2p = Rules.Standard(numPlayers=2)
    val rules3p = Rules.Standard(numPlayers=3)
    val rules4p = Rules.Standard(numPlayers=4)

    val name2p = prefix + "HeuristicStandard2P"
    val games2p = {
      Sim.runMulti(
        name = name2p,
        rules = rules2p,
        numGames,
        runSeed = RandUtils.sha256Long(name2p),
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
        runSeed = RandUtils.sha256Long(name3p),
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
        runSeed = RandUtils.sha256Long(name4p),
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
   [info] Score  0  Games: 10  Percent:  1.0%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.0%
   [info] Score  2  Games: 18  Percent:  1.8%  Cum: 98.4%
   [info] Score  3  Games: 30  Percent:  3.0%  Cum: 96.6%
   [info] Score  4  Games: 32  Percent:  3.2%  Cum: 93.6%
   [info] Score  5  Games: 28  Percent:  2.8%  Cum: 90.4%
   [info] Score  6  Games: 43  Percent:  4.3%  Cum: 87.6%
   [info] Score  7  Games: 36  Percent:  3.6%  Cum: 83.3%
   [info] Score  8  Games: 44  Percent:  4.4%  Cum: 79.7%
   [info] Score  9  Games: 49  Percent:  4.9%  Cum: 75.3%
   [info] Score 10  Games: 53  Percent:  5.3%  Cum: 70.4%
   [info] Score 11  Games: 45  Percent:  4.5%  Cum: 65.1%
   [info] Score 12  Games: 45  Percent:  4.5%  Cum: 60.6%
   [info] Score 13  Games: 35  Percent:  3.5%  Cum: 56.1%
   [info] Score 14  Games: 49  Percent:  4.9%  Cum: 52.6%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 47.7%
   [info] Score 16  Games: 28  Percent:  2.8%  Cum: 44.6%
   [info] Score 17  Games: 27  Percent:  2.7%  Cum: 41.8%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 39.1%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 35.8%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 32.7%
   [info] Score 21  Games: 20  Percent:  2.0%  Cum: 30.8%
   [info] Score 22  Games: 22  Percent:  2.2%  Cum: 28.8%
   [info] Score 23  Games: 61  Percent:  6.1%  Cum: 26.6%
   [info] Score 24  Games: 72  Percent:  7.2%  Cum: 20.5%
   [info] Score 25  Games: 133  Percent: 13.3%  Cum: 13.3%
   [info] Average Score: 14.704
   [info] Average Utility: 36.058
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.8%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 98.9%
   [info] Score  3  Games: 14  Percent:  1.4%  Cum: 98.2%
   [info] Score  4  Games: 17  Percent:  1.7%  Cum: 96.8%
   [info] Score  5  Games: 28  Percent:  2.8%  Cum: 95.1%
   [info] Score  6  Games: 31  Percent:  3.1%  Cum: 92.3%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 89.2%
   [info] Score  8  Games: 27  Percent:  2.7%  Cum: 86.9%
   [info] Score  9  Games: 51  Percent:  5.1%  Cum: 84.2%
   [info] Score 10  Games: 31  Percent:  3.1%  Cum: 79.1%
   [info] Score 11  Games: 59  Percent:  5.9%  Cum: 76.0%
   [info] Score 12  Games: 43  Percent:  4.3%  Cum: 70.1%
   [info] Score 13  Games: 35  Percent:  3.5%  Cum: 65.8%
   [info] Score 14  Games: 54  Percent:  5.4%  Cum: 62.3%
   [info] Score 15  Games: 56  Percent:  5.6%  Cum: 56.9%
   [info] Score 16  Games: 44  Percent:  4.4%  Cum: 51.3%
   [info] Score 17  Games: 59  Percent:  5.9%  Cum: 46.9%
   [info] Score 18  Games: 60  Percent:  6.0%  Cum: 41.0%
   [info] Score 19  Games: 49  Percent:  4.9%  Cum: 35.0%
   [info] Score 20  Games: 51  Percent:  5.1%  Cum: 30.1%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 25.0%
   [info] Score 22  Games: 57  Percent:  5.7%  Cum: 20.2%
   [info] Score 23  Games: 69  Percent:  6.9%  Cum: 14.5%
   [info] Score 24  Games: 49  Percent:  4.9%  Cum:  7.6%
   [info] Score 25  Games: 27  Percent:  2.7%  Cum:  2.7%
   [info] Average Score: 15.259
   [info] Average Utility: 31.868
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  2  Games:  8  Percent:  0.8%  Cum: 99.6%
   [info] Score  3  Games: 13  Percent:  1.3%  Cum: 98.8%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 97.5%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 96.1%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 93.9%
   [info] Score  7  Games: 31  Percent:  3.1%  Cum: 92.5%
   [info] Score  8  Games: 41  Percent:  4.1%  Cum: 89.4%
   [info] Score  9  Games: 53  Percent:  5.3%  Cum: 85.3%
   [info] Score 10  Games: 45  Percent:  4.5%  Cum: 80.0%
   [info] Score 11  Games: 54  Percent:  5.4%  Cum: 75.5%
   [info] Score 12  Games: 56  Percent:  5.6%  Cum: 70.1%
   [info] Score 13  Games: 57  Percent:  5.7%  Cum: 64.5%
   [info] Score 14  Games: 76  Percent:  7.6%  Cum: 58.8%
   [info] Score 15  Games: 66  Percent:  6.6%  Cum: 51.2%
   [info] Score 16  Games: 74  Percent:  7.4%  Cum: 44.6%
   [info] Score 17  Games: 81  Percent:  8.1%  Cum: 37.2%
   [info] Score 18  Games: 70  Percent:  7.0%  Cum: 29.1%
   [info] Score 19  Games: 46  Percent:  4.6%  Cum: 22.1%
   [info] Score 20  Games: 47  Percent:  4.7%  Cum: 17.5%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 12.8%
   [info] Score 22  Games: 41  Percent:  4.1%  Cum: 10.1%
   [info] Score 23  Games: 34  Percent:  3.4%  Cum:  6.0%
   [info] Score 24  Games: 18  Percent:  1.8%  Cum:  2.6%
   [info] Score 25  Games:  8  Percent:  0.8%  Cum:  0.8%
   [info] Average Score: 14.358
   [info] Average Utility: 29.116
   [info]
   [info] Time: 1348.381519065

   */

}
