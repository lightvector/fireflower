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
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.7%
   [info] Score  2  Games:  8  Percent:  0.8%  Cum: 99.1%
   [info] Score  3  Games: 20  Percent:  2.0%  Cum: 98.3%
   [info] Score  4  Games: 30  Percent:  3.0%  Cum: 96.3%
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 93.3%
   [info] Score  6  Games: 34  Percent:  3.4%  Cum: 91.4%
   [info] Score  7  Games: 40  Percent:  4.0%  Cum: 88.0%
   [info] Score  8  Games: 34  Percent:  3.4%  Cum: 84.0%
   [info] Score  9  Games: 51  Percent:  5.1%  Cum: 80.6%
   [info] Score 10  Games: 34  Percent:  3.4%  Cum: 75.5%
   [info] Score 11  Games: 50  Percent:  5.0%  Cum: 72.1%
   [info] Score 12  Games: 41  Percent:  4.1%  Cum: 67.1%
   [info] Score 13  Games: 18  Percent:  1.8%  Cum: 63.0%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 61.2%
   [info] Score 15  Games: 41  Percent:  4.1%  Cum: 57.8%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 53.7%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 51.0%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 48.7%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 46.0%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 42.9%
   [info] Score 21  Games: 24  Percent:  2.4%  Cum: 40.6%
   [info] Score 22  Games: 33  Percent:  3.3%  Cum: 38.2%
   [info] Score 23  Games: 20  Percent:  2.0%  Cum: 34.9%
   [info] Score 24  Games: 49  Percent:  4.9%  Cum: 32.9%
   [info] Score 25  Games: 280  Percent: 28.0%  Cum: 28.0%
   [info] Average Score: 16.443
   [info] Average Utility: 46.886
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.0%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 97.5%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 96.1%
   [info] Score  6  Games: 22  Percent:  2.2%  Cum: 94.1%
   [info] Score  7  Games: 25  Percent:  2.5%  Cum: 91.9%
   [info] Score  8  Games: 14  Percent:  1.4%  Cum: 89.4%
   [info] Score  9  Games: 26  Percent:  2.6%  Cum: 88.0%
   [info] Score 10  Games: 25  Percent:  2.5%  Cum: 85.4%
   [info] Score 11  Games: 32  Percent:  3.2%  Cum: 82.9%
   [info] Score 12  Games: 39  Percent:  3.9%  Cum: 79.7%
   [info] Score 13  Games: 32  Percent:  3.2%  Cum: 75.8%
   [info] Score 14  Games: 28  Percent:  2.8%  Cum: 72.6%
   [info] Score 15  Games: 45  Percent:  4.5%  Cum: 69.8%
   [info] Score 16  Games: 56  Percent:  5.6%  Cum: 65.3%
   [info] Score 17  Games: 70  Percent:  7.0%  Cum: 59.7%
   [info] Score 18  Games: 63  Percent:  6.3%  Cum: 52.7%
   [info] Score 19  Games: 59  Percent:  5.9%  Cum: 46.4%
   [info] Score 20  Games: 45  Percent:  4.5%  Cum: 40.5%
   [info] Score 21  Games: 54  Percent:  5.4%  Cum: 36.0%
   [info] Score 22  Games: 51  Percent:  5.1%  Cum: 30.6%
   [info] Score 23  Games: 74  Percent:  7.4%  Cum: 25.5%
   [info] Score 24  Games: 130  Percent: 13.0%  Cum: 18.1%
   [info] Score 25  Games: 51  Percent:  5.1%  Cum:  5.1%
   [info] Average Score: 17.003
   [info] Average Utility: 36.556
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 99.6%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 99.1%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 97.5%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 95.6%
   [info] Score  7  Games: 17  Percent:  1.7%  Cum: 93.9%
   [info] Score  8  Games: 29  Percent:  2.9%  Cum: 92.2%
   [info] Score  9  Games: 37  Percent:  3.7%  Cum: 89.3%
   [info] Score 10  Games: 49  Percent:  4.9%  Cum: 85.6%
   [info] Score 11  Games: 48  Percent:  4.8%  Cum: 80.7%
   [info] Score 12  Games: 55  Percent:  5.5%  Cum: 75.9%
   [info] Score 13  Games: 54  Percent:  5.4%  Cum: 70.4%
   [info] Score 14  Games: 62  Percent:  6.2%  Cum: 65.0%
   [info] Score 15  Games: 66  Percent:  6.6%  Cum: 58.8%
   [info] Score 16  Games: 58  Percent:  5.8%  Cum: 52.2%
   [info] Score 17  Games: 71  Percent:  7.1%  Cum: 46.4%
   [info] Score 18  Games: 68  Percent:  6.8%  Cum: 39.3%
   [info] Score 19  Games: 52  Percent:  5.2%  Cum: 32.5%
   [info] Score 20  Games: 58  Percent:  5.8%  Cum: 27.3%
   [info] Score 21  Games: 53  Percent:  5.3%  Cum: 21.5%
   [info] Score 22  Games: 47  Percent:  4.7%  Cum: 16.2%
   [info] Score 23  Games: 55  Percent:  5.5%  Cum: 11.5%
   [info] Score 24  Games: 52  Percent:  5.2%  Cum:  6.0%
   [info] Score 25  Games:  8  Percent:  0.8%  Cum:  0.8%
   [info] Average Score: 15.554
   [info] Average Utility: 31.508
   [info]
   [info] Time: 913.533199436

   */

}
