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
   [info] Score  0  Games: 12  Percent:  1.2%  Cum: 100.0%
   [info] Score  1  Games:  8  Percent:  0.8%  Cum: 98.8%
   [info] Score  2  Games: 18  Percent:  1.8%  Cum: 98.0%
   [info] Score  3  Games: 24  Percent:  2.4%  Cum: 96.2%
   [info] Score  4  Games: 33  Percent:  3.3%  Cum: 93.8%
   [info] Score  5  Games: 26  Percent:  2.6%  Cum: 90.5%
   [info] Score  6  Games: 44  Percent:  4.4%  Cum: 87.9%
   [info] Score  7  Games: 41  Percent:  4.1%  Cum: 83.5%
   [info] Score  8  Games: 35  Percent:  3.5%  Cum: 79.4%
   [info] Score  9  Games: 50  Percent:  5.0%  Cum: 75.9%
   [info] Score 10  Games: 37  Percent:  3.7%  Cum: 70.9%
   [info] Score 11  Games: 40  Percent:  4.0%  Cum: 67.2%
   [info] Score 12  Games: 34  Percent:  3.4%  Cum: 63.2%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 59.8%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 56.8%
   [info] Score 15  Games: 30  Percent:  3.0%  Cum: 53.5%
   [info] Score 16  Games: 23  Percent:  2.3%  Cum: 50.5%
   [info] Score 17  Games: 22  Percent:  2.2%  Cum: 48.2%
   [info] Score 18  Games: 24  Percent:  2.4%  Cum: 46.0%
   [info] Score 19  Games: 29  Percent:  2.9%  Cum: 43.6%
   [info] Score 20  Games: 16  Percent:  1.6%  Cum: 40.7%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 39.1%
   [info] Score 22  Games: 33  Percent:  3.3%  Cum: 36.5%
   [info] Score 23  Games: 24  Percent:  2.4%  Cum: 33.2%
   [info] Score 24  Games: 53  Percent:  5.3%  Cum: 30.8%
   [info] Score 25  Games: 255  Percent: 25.5%  Cum: 25.5%
   [info] Average Score: 15.695
   [info] Average Utility: 44.14
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games: 10  Percent:  1.0%  Cum: 99.8%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 98.8%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.1%
   [info] Score  4  Games: 13  Percent:  1.3%  Cum: 96.9%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 95.6%
   [info] Score  6  Games: 19  Percent:  1.9%  Cum: 93.6%
   [info] Score  7  Games: 27  Percent:  2.7%  Cum: 91.7%
   [info] Score  8  Games: 21  Percent:  2.1%  Cum: 89.0%
   [info] Score  9  Games: 34  Percent:  3.4%  Cum: 86.9%
   [info] Score 10  Games: 28  Percent:  2.8%  Cum: 83.5%
   [info] Score 11  Games: 52  Percent:  5.2%  Cum: 80.7%
   [info] Score 12  Games: 36  Percent:  3.6%  Cum: 75.5%
   [info] Score 13  Games: 37  Percent:  3.7%  Cum: 71.9%
   [info] Score 14  Games: 42  Percent:  4.2%  Cum: 68.2%
   [info] Score 15  Games: 48  Percent:  4.8%  Cum: 64.0%
   [info] Score 16  Games: 56  Percent:  5.6%  Cum: 59.2%
   [info] Score 17  Games: 38  Percent:  3.8%  Cum: 53.6%
   [info] Score 18  Games: 61  Percent:  6.1%  Cum: 49.8%
   [info] Score 19  Games: 51  Percent:  5.1%  Cum: 43.7%
   [info] Score 20  Games: 58  Percent:  5.8%  Cum: 38.6%
   [info] Score 21  Games: 65  Percent:  6.5%  Cum: 32.8%
   [info] Score 22  Games: 56  Percent:  5.6%  Cum: 26.3%
   [info] Score 23  Games: 74  Percent:  7.4%  Cum: 20.7%
   [info] Score 24  Games: 92  Percent:  9.2%  Cum: 13.3%
   [info] Score 25  Games: 41  Percent:  4.1%  Cum:  4.1%
   [info] Average Score: 16.363
   [info] Average Utility: 34.776
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  3  Games:  9  Percent:  0.9%  Cum: 99.0%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 98.1%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 97.1%
   [info] Score  6  Games: 15  Percent:  1.5%  Cum: 95.1%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 93.6%
   [info] Score  8  Games: 35  Percent:  3.5%  Cum: 91.3%
   [info] Score  9  Games: 51  Percent:  5.1%  Cum: 87.8%
   [info] Score 10  Games: 53  Percent:  5.3%  Cum: 82.7%
   [info] Score 11  Games: 56  Percent:  5.6%  Cum: 77.4%
   [info] Score 12  Games: 58  Percent:  5.8%  Cum: 71.8%
   [info] Score 13  Games: 57  Percent:  5.7%  Cum: 66.0%
   [info] Score 14  Games: 77  Percent:  7.7%  Cum: 60.3%
   [info] Score 15  Games: 63  Percent:  6.3%  Cum: 52.6%
   [info] Score 16  Games: 72  Percent:  7.2%  Cum: 46.3%
   [info] Score 17  Games: 63  Percent:  6.3%  Cum: 39.1%
   [info] Score 18  Games: 70  Percent:  7.0%  Cum: 32.8%
   [info] Score 19  Games: 49  Percent:  4.9%  Cum: 25.8%
   [info] Score 20  Games: 46  Percent:  4.6%  Cum: 20.9%
   [info] Score 21  Games: 35  Percent:  3.5%  Cum: 16.3%
   [info] Score 22  Games: 50  Percent:  5.0%  Cum: 12.8%
   [info] Score 23  Games: 44  Percent:  4.4%  Cum:  7.8%
   [info] Score 24  Games: 32  Percent:  3.2%  Cum:  3.4%
   [info] Score 25  Games:  2  Percent:  0.2%  Cum:  0.2%
   [info] Average Score: 14.776
   [info] Average Utility: 29.652
   [info]
   [info] Time: 707.331848303

   */

}
