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
   [info] Score  0  Games: 13  Percent:  1.3%  Cum: 100.0%
   [info] Score  1  Games:  8  Percent:  0.8%  Cum: 98.7%
   [info] Score  2  Games: 13  Percent:  1.3%  Cum: 97.9%
   [info] Score  3  Games: 18  Percent:  1.8%  Cum: 96.6%
   [info] Score  4  Games: 31  Percent:  3.1%  Cum: 94.8%
   [info] Score  5  Games: 25  Percent:  2.5%  Cum: 91.7%
   [info] Score  6  Games: 39  Percent:  3.9%  Cum: 89.2%
   [info] Score  7  Games: 37  Percent:  3.7%  Cum: 85.3%
   [info] Score  8  Games: 32  Percent:  3.2%  Cum: 81.6%
   [info] Score  9  Games: 48  Percent:  4.8%  Cum: 78.4%
   [info] Score 10  Games: 36  Percent:  3.6%  Cum: 73.6%
   [info] Score 11  Games: 39  Percent:  3.9%  Cum: 70.0%
   [info] Score 12  Games: 43  Percent:  4.3%  Cum: 66.1%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 61.8%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 59.7%
   [info] Score 15  Games: 32  Percent:  3.2%  Cum: 56.4%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 53.2%
   [info] Score 17  Games: 20  Percent:  2.0%  Cum: 50.3%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 48.3%
   [info] Score 19  Games: 34  Percent:  3.4%  Cum: 45.6%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 42.2%
   [info] Score 21  Games: 23  Percent:  2.3%  Cum: 40.3%
   [info] Score 22  Games: 28  Percent:  2.8%  Cum: 38.0%
   [info] Score 23  Games: 32  Percent:  3.2%  Cum: 35.2%
   [info] Score 24  Games: 49  Percent:  4.9%  Cum: 32.0%
   [info] Score 25  Games: 271  Percent: 27.1%  Cum: 27.1%
   [info] Average Score: 16.14
   [info] Average Utility: 45.83
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.0%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 98.4%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 97.6%
   [info] Score  5  Games: 23  Percent:  2.3%  Cum: 96.4%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 94.1%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 92.5%
   [info] Score  8  Games: 14  Percent:  1.4%  Cum: 90.1%
   [info] Score  9  Games: 33  Percent:  3.3%  Cum: 88.7%
   [info] Score 10  Games: 31  Percent:  3.1%  Cum: 85.4%
   [info] Score 11  Games: 37  Percent:  3.7%  Cum: 82.3%
   [info] Score 12  Games: 40  Percent:  4.0%  Cum: 78.6%
   [info] Score 13  Games: 38  Percent:  3.8%  Cum: 74.6%
   [info] Score 14  Games: 37  Percent:  3.7%  Cum: 70.8%
   [info] Score 15  Games: 44  Percent:  4.4%  Cum: 67.1%
   [info] Score 16  Games: 56  Percent:  5.6%  Cum: 62.7%
   [info] Score 17  Games: 52  Percent:  5.2%  Cum: 57.1%
   [info] Score 18  Games: 68  Percent:  6.8%  Cum: 51.9%
   [info] Score 19  Games: 51  Percent:  5.1%  Cum: 45.1%
   [info] Score 20  Games: 54  Percent:  5.4%  Cum: 40.0%
   [info] Score 21  Games: 64  Percent:  6.4%  Cum: 34.6%
   [info] Score 22  Games: 54  Percent:  5.4%  Cum: 28.2%
   [info] Score 23  Games: 83  Percent:  8.3%  Cum: 22.8%
   [info] Score 24  Games: 103  Percent: 10.3%  Cum: 14.5%
   [info] Score 25  Games: 42  Percent:  4.2%  Cum:  4.2%
   [info] Average Score: 16.766
   [info] Average Utility: 35.632
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.8%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 97.8%
   [info] Score  5  Games: 18  Percent:  1.8%  Cum: 96.8%
   [info] Score  6  Games: 13  Percent:  1.3%  Cum: 95.0%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 93.7%
   [info] Score  8  Games: 33  Percent:  3.3%  Cum: 91.3%
   [info] Score  9  Games: 46  Percent:  4.6%  Cum: 88.0%
   [info] Score 10  Games: 47  Percent:  4.7%  Cum: 83.4%
   [info] Score 11  Games: 57  Percent:  5.7%  Cum: 78.7%
   [info] Score 12  Games: 57  Percent:  5.7%  Cum: 73.0%
   [info] Score 13  Games: 64  Percent:  6.4%  Cum: 67.3%
   [info] Score 14  Games: 67  Percent:  6.7%  Cum: 60.9%
   [info] Score 15  Games: 64  Percent:  6.4%  Cum: 54.2%
   [info] Score 16  Games: 67  Percent:  6.7%  Cum: 47.8%
   [info] Score 17  Games: 71  Percent:  7.1%  Cum: 41.1%
   [info] Score 18  Games: 73  Percent:  7.3%  Cum: 34.0%
   [info] Score 19  Games: 56  Percent:  5.6%  Cum: 26.7%
   [info] Score 20  Games: 38  Percent:  3.8%  Cum: 21.1%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 17.3%
   [info] Score 22  Games: 46  Percent:  4.6%  Cum: 12.7%
   [info] Score 23  Games: 48  Percent:  4.8%  Cum:  8.1%
   [info] Score 24  Games: 28  Percent:  2.8%  Cum:  3.3%
   [info] Score 25  Games:  5  Percent:  0.5%  Cum:  0.5%
   [info] Average Score: 14.907
   [info] Average Utility: 30.064
   [info]
   [info] Time: 924.461486112

   */

}
