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
   [info] Score  3  Games: 22  Percent:  2.2%  Cum: 96.2%
   [info] Score  4  Games: 40  Percent:  4.0%  Cum: 94.0%
   [info] Score  5  Games: 26  Percent:  2.6%  Cum: 90.0%
   [info] Score  6  Games: 44  Percent:  4.4%  Cum: 87.4%
   [info] Score  7  Games: 48  Percent:  4.8%  Cum: 83.0%
   [info] Score  8  Games: 38  Percent:  3.8%  Cum: 78.2%
   [info] Score  9  Games: 48  Percent:  4.8%  Cum: 74.4%
   [info] Score 10  Games: 44  Percent:  4.4%  Cum: 69.6%
   [info] Score 11  Games: 39  Percent:  3.9%  Cum: 65.2%
   [info] Score 12  Games: 42  Percent:  4.2%  Cum: 61.3%
   [info] Score 13  Games: 31  Percent:  3.1%  Cum: 57.1%
   [info] Score 14  Games: 36  Percent:  3.6%  Cum: 54.0%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 50.4%
   [info] Score 16  Games: 34  Percent:  3.4%  Cum: 47.3%
   [info] Score 17  Games: 16  Percent:  1.6%  Cum: 43.9%
   [info] Score 18  Games: 21  Percent:  2.1%  Cum: 42.3%
   [info] Score 19  Games: 29  Percent:  2.9%  Cum: 40.2%
   [info] Score 20  Games: 20  Percent:  2.0%  Cum: 37.3%
   [info] Score 21  Games: 34  Percent:  3.4%  Cum: 35.3%
   [info] Score 22  Games: 27  Percent:  2.7%  Cum: 31.9%
   [info] Score 23  Games: 14  Percent:  1.4%  Cum: 29.2%
   [info] Score 24  Games: 54  Percent:  5.4%  Cum: 27.8%
   [info] Score 25  Games: 224  Percent: 22.4%  Cum: 22.4%
   [info] Average Score: 15.152
   [info] Average Utility: 41.504
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games: 10  Percent:  1.0%  Cum: 99.8%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 98.8%
   [info] Score  3  Games: 11  Percent:  1.1%  Cum: 98.1%
   [info] Score  4  Games: 15  Percent:  1.5%  Cum: 97.0%
   [info] Score  5  Games: 18  Percent:  1.8%  Cum: 95.5%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 93.7%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 91.9%
   [info] Score  8  Games: 24  Percent:  2.4%  Cum: 89.5%
   [info] Score  9  Games: 31  Percent:  3.1%  Cum: 87.1%
   [info] Score 10  Games: 38  Percent:  3.8%  Cum: 84.0%
   [info] Score 11  Games: 50  Percent:  5.0%  Cum: 80.2%
   [info] Score 12  Games: 40  Percent:  4.0%  Cum: 75.2%
   [info] Score 13  Games: 43  Percent:  4.3%  Cum: 71.2%
   [info] Score 14  Games: 45  Percent:  4.5%  Cum: 66.9%
   [info] Score 15  Games: 46  Percent:  4.6%  Cum: 62.4%
   [info] Score 16  Games: 57  Percent:  5.7%  Cum: 57.8%
   [info] Score 17  Games: 63  Percent:  6.3%  Cum: 52.1%
   [info] Score 18  Games: 57  Percent:  5.7%  Cum: 45.8%
   [info] Score 19  Games: 46  Percent:  4.6%  Cum: 40.1%
   [info] Score 20  Games: 59  Percent:  5.9%  Cum: 35.5%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 29.6%
   [info] Score 22  Games: 53  Percent:  5.3%  Cum: 24.8%
   [info] Score 23  Games: 62  Percent:  6.2%  Cum: 19.5%
   [info] Score 24  Games: 98  Percent:  9.8%  Cum: 13.3%
   [info] Score 25  Games: 35  Percent:  3.5%  Cum:  3.5%
   [info] Average Score: 16.133
   [info] Average Utility: 34.016
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  3  Games:  9  Percent:  0.9%  Cum: 99.0%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 98.1%
   [info] Score  5  Games: 18  Percent:  1.8%  Cum: 97.1%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 95.3%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 93.6%
   [info] Score  8  Games: 27  Percent:  2.7%  Cum: 91.4%
   [info] Score  9  Games: 57  Percent:  5.7%  Cum: 88.7%
   [info] Score 10  Games: 58  Percent:  5.8%  Cum: 83.0%
   [info] Score 11  Games: 59  Percent:  5.9%  Cum: 77.2%
   [info] Score 12  Games: 57  Percent:  5.7%  Cum: 71.3%
   [info] Score 13  Games: 64  Percent:  6.4%  Cum: 65.6%
   [info] Score 14  Games: 73  Percent:  7.3%  Cum: 59.2%
   [info] Score 15  Games: 68  Percent:  6.8%  Cum: 51.9%
   [info] Score 16  Games: 61  Percent:  6.1%  Cum: 45.1%
   [info] Score 17  Games: 66  Percent:  6.6%  Cum: 39.0%
   [info] Score 18  Games: 62  Percent:  6.2%  Cum: 32.4%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 26.2%
   [info] Score 20  Games: 42  Percent:  4.2%  Cum: 21.2%
   [info] Score 21  Games: 37  Percent:  3.7%  Cum: 17.0%
   [info] Score 22  Games: 51  Percent:  5.1%  Cum: 13.3%
   [info] Score 23  Games: 46  Percent:  4.6%  Cum:  8.2%
   [info] Score 24  Games: 34  Percent:  3.4%  Cum:  3.6%
   [info] Score 25  Games:  2  Percent:  0.2%  Cum:  0.2%
   [info] Average Score: 14.77
   [info] Average Utility: 29.64
   [info]
   [info] Time: 682.84653775

   */

}
