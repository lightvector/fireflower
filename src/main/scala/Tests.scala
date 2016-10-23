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
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  2  Games: 16  Percent:  1.6%  Cum: 98.6%
   [info] Score  3  Games: 30  Percent:  3.0%  Cum: 97.0%
   [info] Score  4  Games: 34  Percent:  3.4%  Cum: 94.0%
   [info] Score  5  Games: 25  Percent:  2.5%  Cum: 90.6%
   [info] Score  6  Games: 43  Percent:  4.3%  Cum: 88.1%
   [info] Score  7  Games: 33  Percent:  3.3%  Cum: 83.8%
   [info] Score  8  Games: 46  Percent:  4.6%  Cum: 80.5%
   [info] Score  9  Games: 54  Percent:  5.4%  Cum: 75.9%
   [info] Score 10  Games: 51  Percent:  5.1%  Cum: 70.5%
   [info] Score 11  Games: 50  Percent:  5.0%  Cum: 65.4%
   [info] Score 12  Games: 49  Percent:  4.9%  Cum: 60.4%
   [info] Score 13  Games: 41  Percent:  4.1%  Cum: 55.5%
   [info] Score 14  Games: 43  Percent:  4.3%  Cum: 51.4%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 47.1%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 44.0%
   [info] Score 17  Games: 32  Percent:  3.2%  Cum: 40.9%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 37.7%
   [info] Score 19  Games: 34  Percent:  3.4%  Cum: 34.4%
   [info] Score 20  Games: 18  Percent:  1.8%  Cum: 31.0%
   [info] Score 21  Games: 16  Percent:  1.6%  Cum: 29.2%
   [info] Score 22  Games: 26  Percent:  2.6%  Cum: 27.6%
   [info] Score 23  Games: 54  Percent:  5.4%  Cum: 25.0%
   [info] Score 24  Games: 126  Percent: 12.6%  Cum: 19.6%
   [info] Score 25  Games: 70  Percent:  7.0%  Cum:  7.0%
   [info] Average Score: 14.542
   [info] Average Utility: 32.584
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  8  Percent:  0.8%  Cum: 99.9%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.1%
   [info] Score  3  Games: 15  Percent:  1.5%  Cum: 98.5%
   [info] Score  4  Games: 19  Percent:  1.9%  Cum: 97.0%
   [info] Score  5  Games: 26  Percent:  2.6%  Cum: 95.1%
   [info] Score  6  Games: 28  Percent:  2.8%  Cum: 92.5%
   [info] Score  7  Games: 25  Percent:  2.5%  Cum: 89.7%
   [info] Score  8  Games: 27  Percent:  2.7%  Cum: 87.2%
   [info] Score  9  Games: 38  Percent:  3.8%  Cum: 84.5%
   [info] Score 10  Games: 41  Percent:  4.1%  Cum: 80.7%
   [info] Score 11  Games: 47  Percent:  4.7%  Cum: 76.6%
   [info] Score 12  Games: 40  Percent:  4.0%  Cum: 71.9%
   [info] Score 13  Games: 44  Percent:  4.4%  Cum: 67.9%
   [info] Score 14  Games: 40  Percent:  4.0%  Cum: 63.5%
   [info] Score 15  Games: 50  Percent:  5.0%  Cum: 59.5%
   [info] Score 16  Games: 49  Percent:  4.9%  Cum: 54.5%
   [info] Score 17  Games: 56  Percent:  5.6%  Cum: 49.6%
   [info] Score 18  Games: 63  Percent:  6.3%  Cum: 44.0%
   [info] Score 19  Games: 55  Percent:  5.5%  Cum: 37.7%
   [info] Score 20  Games: 54  Percent:  5.4%  Cum: 32.2%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 26.8%
   [info] Score 22  Games: 59  Percent:  5.9%  Cum: 22.0%
   [info] Score 23  Games: 82  Percent:  8.2%  Cum: 16.1%
   [info] Score 24  Games: 57  Percent:  5.7%  Cum:  7.9%
   [info] Score 25  Games: 22  Percent:  2.2%  Cum:  2.2%
   [info] Average Score: 15.566
   [info] Average Utility: 32.232
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  2  Games: 10  Percent:  1.0%  Cum: 99.6%
   [info] Score  3  Games: 14  Percent:  1.4%  Cum: 98.6%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 97.2%
   [info] Score  5  Games: 17  Percent:  1.7%  Cum: 95.8%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 94.1%
   [info] Score  7  Games: 27  Percent:  2.7%  Cum: 92.5%
   [info] Score  8  Games: 36  Percent:  3.6%  Cum: 89.8%
   [info] Score  9  Games: 53  Percent:  5.3%  Cum: 86.2%
   [info] Score 10  Games: 46  Percent:  4.6%  Cum: 80.9%
   [info] Score 11  Games: 55  Percent:  5.5%  Cum: 76.3%
   [info] Score 12  Games: 49  Percent:  4.9%  Cum: 70.8%
   [info] Score 13  Games: 57  Percent:  5.7%  Cum: 65.9%
   [info] Score 14  Games: 69  Percent:  6.9%  Cum: 60.2%
   [info] Score 15  Games: 73  Percent:  7.3%  Cum: 53.3%
   [info] Score 16  Games: 84  Percent:  8.4%  Cum: 46.0%
   [info] Score 17  Games: 71  Percent:  7.1%  Cum: 37.6%
   [info] Score 18  Games: 68  Percent:  6.8%  Cum: 30.5%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 23.7%
   [info] Score 20  Games: 49  Percent:  4.9%  Cum: 18.7%
   [info] Score 21  Games: 35  Percent:  3.5%  Cum: 13.8%
   [info] Score 22  Games: 42  Percent:  4.2%  Cum: 10.3%
   [info] Score 23  Games: 35  Percent:  3.5%  Cum:  6.1%
   [info] Score 24  Games: 22  Percent:  2.2%  Cum:  2.6%
   [info] Score 25  Games:  4  Percent:  0.4%  Cum:  0.4%
   [info] Average Score: 14.507
   [info] Average Utility: 29.214
   [info]
   [info] Time: 1393.003562573

   */

}
