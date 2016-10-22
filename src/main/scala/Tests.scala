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
   [info] Score  2  Games: 11  Percent:  1.1%  Cum: 98.6%
   [info] Score  3  Games: 26  Percent:  2.6%  Cum: 97.5%
   [info] Score  4  Games: 26  Percent:  2.6%  Cum: 94.9%
   [info] Score  5  Games: 29  Percent:  2.9%  Cum: 92.3%
   [info] Score  6  Games: 41  Percent:  4.1%  Cum: 89.4%
   [info] Score  7  Games: 31  Percent:  3.1%  Cum: 85.3%
   [info] Score  8  Games: 32  Percent:  3.2%  Cum: 82.2%
   [info] Score  9  Games: 50  Percent:  5.0%  Cum: 79.0%
   [info] Score 10  Games: 53  Percent:  5.3%  Cum: 74.0%
   [info] Score 11  Games: 43  Percent:  4.3%  Cum: 68.7%
   [info] Score 12  Games: 53  Percent:  5.3%  Cum: 64.4%
   [info] Score 13  Games: 48  Percent:  4.8%  Cum: 59.1%
   [info] Score 14  Games: 45  Percent:  4.5%  Cum: 54.3%
   [info] Score 15  Games: 34  Percent:  3.4%  Cum: 49.8%
   [info] Score 16  Games: 35  Percent:  3.5%  Cum: 46.4%
   [info] Score 17  Games: 27  Percent:  2.7%  Cum: 42.9%
   [info] Score 18  Games: 40  Percent:  4.0%  Cum: 40.2%
   [info] Score 19  Games: 36  Percent:  3.6%  Cum: 36.2%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 32.6%
   [info] Score 21  Games: 21  Percent:  2.1%  Cum: 30.7%
   [info] Score 22  Games: 28  Percent:  2.8%  Cum: 28.6%
   [info] Score 23  Games: 56  Percent:  5.6%  Cum: 25.8%
   [info] Score 24  Games: 128  Percent: 12.8%  Cum: 20.2%
   [info] Score 25  Games: 74  Percent:  7.4%  Cum:  7.4%
   [info] Average Score: 14.995
   [info] Average Utility: 33.69
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 99.0%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.5%
   [info] Score  4  Games: 20  Percent:  2.0%  Cum: 96.9%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 94.9%
   [info] Score  6  Games: 21  Percent:  2.1%  Cum: 92.9%
   [info] Score  7  Games: 21  Percent:  2.1%  Cum: 90.8%
   [info] Score  8  Games: 29  Percent:  2.9%  Cum: 88.7%
   [info] Score  9  Games: 38  Percent:  3.8%  Cum: 85.8%
   [info] Score 10  Games: 46  Percent:  4.6%  Cum: 82.0%
   [info] Score 11  Games: 55  Percent:  5.5%  Cum: 77.4%
   [info] Score 12  Games: 46  Percent:  4.6%  Cum: 71.9%
   [info] Score 13  Games: 43  Percent:  4.3%  Cum: 67.3%
   [info] Score 14  Games: 56  Percent:  5.6%  Cum: 63.0%
   [info] Score 15  Games: 54  Percent:  5.4%  Cum: 57.4%
   [info] Score 16  Games: 44  Percent:  4.4%  Cum: 52.0%
   [info] Score 17  Games: 64  Percent:  6.4%  Cum: 47.6%
   [info] Score 18  Games: 67  Percent:  6.7%  Cum: 41.2%
   [info] Score 19  Games: 56  Percent:  5.6%  Cum: 34.5%
   [info] Score 20  Games: 48  Percent:  4.8%  Cum: 28.9%
   [info] Score 21  Games: 59  Percent:  5.9%  Cum: 24.1%
   [info] Score 22  Games: 54  Percent:  5.4%  Cum: 18.2%
   [info] Score 23  Games: 64  Percent:  6.4%  Cum: 12.8%
   [info] Score 24  Games: 43  Percent:  4.3%  Cum:  6.4%
   [info] Score 25  Games: 21  Percent:  2.1%  Cum:  2.1%
   [info] Average Score: 15.342
   [info] Average Utility: 31.734
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.5%
   [info] Score  3  Games: 14  Percent:  1.4%  Cum: 98.6%
   [info] Score  4  Games:  9  Percent:  0.9%  Cum: 97.2%
   [info] Score  5  Games: 10  Percent:  1.0%  Cum: 96.3%
   [info] Score  6  Games: 23  Percent:  2.3%  Cum: 95.3%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 93.0%
   [info] Score  8  Games: 41  Percent:  4.1%  Cum: 90.7%
   [info] Score  9  Games: 57  Percent:  5.7%  Cum: 86.6%
   [info] Score 10  Games: 42  Percent:  4.2%  Cum: 80.9%
   [info] Score 11  Games: 52  Percent:  5.2%  Cum: 76.7%
   [info] Score 12  Games: 79  Percent:  7.9%  Cum: 71.5%
   [info] Score 13  Games: 65  Percent:  6.5%  Cum: 63.6%
   [info] Score 14  Games: 77  Percent:  7.7%  Cum: 57.1%
   [info] Score 15  Games: 62  Percent:  6.2%  Cum: 49.4%
   [info] Score 16  Games: 90  Percent:  9.0%  Cum: 43.2%
   [info] Score 17  Games: 67  Percent:  6.7%  Cum: 34.2%
   [info] Score 18  Games: 63  Percent:  6.3%  Cum: 27.5%
   [info] Score 19  Games: 49  Percent:  4.9%  Cum: 21.2%
   [info] Score 20  Games: 38  Percent:  3.8%  Cum: 16.3%
   [info] Score 21  Games: 32  Percent:  3.2%  Cum: 12.5%
   [info] Score 22  Games: 32  Percent:  3.2%  Cum:  9.3%
   [info] Score 23  Games: 34  Percent:  3.4%  Cum:  6.1%
   [info] Score 24  Games: 23  Percent:  2.3%  Cum:  2.7%
   [info] Score 25  Games:  4  Percent:  0.4%  Cum:  0.4%
   [info] Average Score: 14.296
   [info] Average Utility: 28.792
   [info]
   [info] Time: 900.964417686

   */

}
