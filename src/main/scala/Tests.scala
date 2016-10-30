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
   [info] Score  3  Games: 17  Percent:  1.7%  Cum: 96.6%
   [info] Score  4  Games: 31  Percent:  3.1%  Cum: 94.9%
   [info] Score  5  Games: 25  Percent:  2.5%  Cum: 91.8%
   [info] Score  6  Games: 39  Percent:  3.9%  Cum: 89.3%
   [info] Score  7  Games: 38  Percent:  3.8%  Cum: 85.4%
   [info] Score  8  Games: 33  Percent:  3.3%  Cum: 81.6%
   [info] Score  9  Games: 48  Percent:  4.8%  Cum: 78.3%
   [info] Score 10  Games: 35  Percent:  3.5%  Cum: 73.5%
   [info] Score 11  Games: 40  Percent:  4.0%  Cum: 70.0%
   [info] Score 12  Games: 42  Percent:  4.2%  Cum: 66.0%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 61.8%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 59.7%
   [info] Score 15  Games: 33  Percent:  3.3%  Cum: 56.4%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 53.1%
   [info] Score 17  Games: 21  Percent:  2.1%  Cum: 50.2%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 48.1%
   [info] Score 19  Games: 34  Percent:  3.4%  Cum: 45.5%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 42.1%
   [info] Score 21  Games: 21  Percent:  2.1%  Cum: 40.2%
   [info] Score 22  Games: 29  Percent:  2.9%  Cum: 38.1%
   [info] Score 23  Games: 32  Percent:  3.2%  Cum: 35.2%
   [info] Score 24  Games: 49  Percent:  4.9%  Cum: 32.0%
   [info] Score 25  Games: 271  Percent: 27.1%  Cum: 27.1%
   [info] Average Score: 16.135
   [info] Average Utility: 45.82
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.0%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 98.4%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 97.6%
   [info] Score  5  Games: 23  Percent:  2.3%  Cum: 96.4%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 94.1%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 92.4%
   [info] Score  8  Games: 14  Percent:  1.4%  Cum: 90.0%
   [info] Score  9  Games: 33  Percent:  3.3%  Cum: 88.6%
   [info] Score 10  Games: 30  Percent:  3.0%  Cum: 85.3%
   [info] Score 11  Games: 38  Percent:  3.8%  Cum: 82.3%
   [info] Score 12  Games: 42  Percent:  4.2%  Cum: 78.5%
   [info] Score 13  Games: 37  Percent:  3.7%  Cum: 74.3%
   [info] Score 14  Games: 36  Percent:  3.6%  Cum: 70.6%
   [info] Score 15  Games: 45  Percent:  4.5%  Cum: 67.0%
   [info] Score 16  Games: 54  Percent:  5.4%  Cum: 62.5%
   [info] Score 17  Games: 59  Percent:  5.9%  Cum: 57.1%
   [info] Score 18  Games: 67  Percent:  6.7%  Cum: 51.2%
   [info] Score 19  Games: 54  Percent:  5.4%  Cum: 44.5%
   [info] Score 20  Games: 50  Percent:  5.0%  Cum: 39.1%
   [info] Score 21  Games: 59  Percent:  5.9%  Cum: 34.1%
   [info] Score 22  Games: 58  Percent:  5.8%  Cum: 28.2%
   [info] Score 23  Games: 82  Percent:  8.2%  Cum: 22.4%
   [info] Score 24  Games: 102  Percent: 10.2%  Cum: 14.2%
   [info] Score 25  Games: 40  Percent:  4.0%  Cum:  4.0%
   [info] Average Score: 16.717
   [info] Average Utility: 35.434
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.8%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 97.8%
   [info] Score  5  Games: 18  Percent:  1.8%  Cum: 96.8%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 95.0%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 93.6%
   [info] Score  8  Games: 31  Percent:  3.1%  Cum: 91.3%
   [info] Score  9  Games: 49  Percent:  4.9%  Cum: 88.2%
   [info] Score 10  Games: 43  Percent:  4.3%  Cum: 83.3%
   [info] Score 11  Games: 60  Percent:  6.0%  Cum: 79.0%
   [info] Score 12  Games: 54  Percent:  5.4%  Cum: 73.0%
   [info] Score 13  Games: 67  Percent:  6.7%  Cum: 67.6%
   [info] Score 14  Games: 65  Percent:  6.5%  Cum: 60.9%
   [info] Score 15  Games: 65  Percent:  6.5%  Cum: 54.4%
   [info] Score 16  Games: 64  Percent:  6.4%  Cum: 47.9%
   [info] Score 17  Games: 70  Percent:  7.0%  Cum: 41.5%
   [info] Score 18  Games: 75  Percent:  7.5%  Cum: 34.5%
   [info] Score 19  Games: 59  Percent:  5.9%  Cum: 27.0%
   [info] Score 20  Games: 39  Percent:  3.9%  Cum: 21.1%
   [info] Score 21  Games: 44  Percent:  4.4%  Cum: 17.2%
   [info] Score 22  Games: 49  Percent:  4.9%  Cum: 12.8%
   [info] Score 23  Games: 48  Percent:  4.8%  Cum:  7.9%
   [info] Score 24  Games: 26  Percent:  2.6%  Cum:  3.1%
   [info] Score 25  Games:  5  Percent:  0.5%  Cum:  0.5%
   [info] Average Score: 14.924
   [info] Average Utility: 30.098
   [info]
   [info] Time: 918.379660607

   */

}
