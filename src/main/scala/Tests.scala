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
   [info] Score  2  Games: 15  Percent:  1.5%  Cum: 98.4%
   [info] Score  3  Games: 26  Percent:  2.6%  Cum: 96.9%
   [info] Score  4  Games: 30  Percent:  3.0%  Cum: 94.3%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 91.3%
   [info] Score  6  Games: 39  Percent:  3.9%  Cum: 89.1%
   [info] Score  7  Games: 33  Percent:  3.3%  Cum: 85.2%
   [info] Score  8  Games: 44  Percent:  4.4%  Cum: 81.9%
   [info] Score  9  Games: 50  Percent:  5.0%  Cum: 77.5%
   [info] Score 10  Games: 55  Percent:  5.5%  Cum: 72.5%
   [info] Score 11  Games: 45  Percent:  4.5%  Cum: 67.0%
   [info] Score 12  Games: 49  Percent:  4.9%  Cum: 62.5%
   [info] Score 13  Games: 33  Percent:  3.3%  Cum: 57.6%
   [info] Score 14  Games: 47  Percent:  4.7%  Cum: 54.3%
   [info] Score 15  Games: 37  Percent:  3.7%  Cum: 49.6%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 45.9%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 42.8%
   [info] Score 18  Games: 34  Percent:  3.4%  Cum: 40.3%
   [info] Score 19  Games: 39  Percent:  3.9%  Cum: 36.9%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 33.0%
   [info] Score 21  Games: 23  Percent:  2.3%  Cum: 31.1%
   [info] Score 22  Games: 24  Percent:  2.4%  Cum: 28.8%
   [info] Score 23  Games: 53  Percent:  5.3%  Cum: 26.4%
   [info] Score 24  Games: 67  Percent:  6.7%  Cum: 21.1%
   [info] Score 25  Games: 144  Percent: 14.4%  Cum: 14.4%
   [info] Average Score: 14.978
   [info] Average Utility: 37.156
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 98.9%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.3%
   [info] Score  4  Games: 16  Percent:  1.6%  Cum: 97.3%
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 95.7%
   [info] Score  6  Games: 22  Percent:  2.2%  Cum: 93.8%
   [info] Score  7  Games: 21  Percent:  2.1%  Cum: 91.6%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 89.5%
   [info] Score  9  Games: 39  Percent:  3.9%  Cum: 86.7%
   [info] Score 10  Games: 34  Percent:  3.4%  Cum: 82.8%
   [info] Score 11  Games: 51  Percent:  5.1%  Cum: 79.4%
   [info] Score 12  Games: 55  Percent:  5.5%  Cum: 74.3%
   [info] Score 13  Games: 35  Percent:  3.5%  Cum: 68.8%
   [info] Score 14  Games: 54  Percent:  5.4%  Cum: 65.3%
   [info] Score 15  Games: 49  Percent:  4.9%  Cum: 59.9%
   [info] Score 16  Games: 59  Percent:  5.9%  Cum: 55.0%
   [info] Score 17  Games: 58  Percent:  5.8%  Cum: 49.1%
   [info] Score 18  Games: 60  Percent:  6.0%  Cum: 43.3%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 37.3%
   [info] Score 20  Games: 55  Percent:  5.5%  Cum: 32.3%
   [info] Score 21  Games: 56  Percent:  5.6%  Cum: 26.8%
   [info] Score 22  Games: 61  Percent:  6.1%  Cum: 21.2%
   [info] Score 23  Games: 76  Percent:  7.6%  Cum: 15.1%
   [info] Score 24  Games: 46  Percent:  4.6%  Cum:  7.5%
   [info] Score 25  Games: 29  Percent:  2.9%  Cum:  2.9%
   [info] Average Score: 15.726
   [info] Average Utility: 32.902
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.6%
   [info] Score  3  Games: 14  Percent:  1.4%  Cum: 98.9%
   [info] Score  4  Games: 13  Percent:  1.3%  Cum: 97.5%
   [info] Score  5  Games: 18  Percent:  1.8%  Cum: 96.2%
   [info] Score  6  Games: 13  Percent:  1.3%  Cum: 94.4%
   [info] Score  7  Games: 25  Percent:  2.5%  Cum: 93.1%
   [info] Score  8  Games: 34  Percent:  3.4%  Cum: 90.6%
   [info] Score  9  Games: 55  Percent:  5.5%  Cum: 87.2%
   [info] Score 10  Games: 43  Percent:  4.3%  Cum: 81.7%
   [info] Score 11  Games: 56  Percent:  5.6%  Cum: 77.4%
   [info] Score 12  Games: 60  Percent:  6.0%  Cum: 71.8%
   [info] Score 13  Games: 64  Percent:  6.4%  Cum: 65.8%
   [info] Score 14  Games: 79  Percent:  7.9%  Cum: 59.4%
   [info] Score 15  Games: 71  Percent:  7.1%  Cum: 51.5%
   [info] Score 16  Games: 79  Percent:  7.9%  Cum: 44.4%
   [info] Score 17  Games: 83  Percent:  8.3%  Cum: 36.5%
   [info] Score 18  Games: 68  Percent:  6.8%  Cum: 28.2%
   [info] Score 19  Games: 47  Percent:  4.7%  Cum: 21.4%
   [info] Score 20  Games: 42  Percent:  4.2%  Cum: 16.7%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 12.5%
   [info] Score 22  Games: 45  Percent:  4.5%  Cum:  9.9%
   [info] Score 23  Games: 30  Percent:  3.0%  Cum:  5.4%
   [info] Score 24  Games: 18  Percent:  1.8%  Cum:  2.4%
   [info] Score 25  Games:  6  Percent:  0.6%  Cum:  0.6%
   [info] Average Score: 14.429
   [info] Average Utility: 29.158
   [info]
   [info] Time: 1371.877464837

   */

}
