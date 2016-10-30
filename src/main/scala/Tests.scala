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
   [info] Score  0  Games: 14  Percent:  1.4%  Cum: 100.0%
   [info] Score  1  Games:  8  Percent:  0.8%  Cum: 98.6%
   [info] Score  2  Games: 17  Percent:  1.7%  Cum: 97.8%
   [info] Score  3  Games: 21  Percent:  2.1%  Cum: 96.1%
   [info] Score  4  Games: 36  Percent:  3.6%  Cum: 94.0%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 90.4%
   [info] Score  6  Games: 43  Percent:  4.3%  Cum: 88.4%
   [info] Score  7  Games: 40  Percent:  4.0%  Cum: 84.1%
   [info] Score  8  Games: 35  Percent:  3.5%  Cum: 80.1%
   [info] Score  9  Games: 46  Percent:  4.6%  Cum: 76.6%
   [info] Score 10  Games: 33  Percent:  3.3%  Cum: 72.0%
   [info] Score 11  Games: 40  Percent:  4.0%  Cum: 68.7%
   [info] Score 12  Games: 38  Percent:  3.8%  Cum: 64.7%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 60.9%
   [info] Score 14  Games: 35  Percent:  3.5%  Cum: 58.8%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 55.3%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 52.4%
   [info] Score 17  Games: 26  Percent:  2.6%  Cum: 49.5%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 46.9%
   [info] Score 19  Games: 33  Percent:  3.3%  Cum: 44.3%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 41.0%
   [info] Score 21  Games: 22  Percent:  2.2%  Cum: 39.1%
   [info] Score 22  Games: 28  Percent:  2.8%  Cum: 36.9%
   [info] Score 23  Games: 34  Percent:  3.4%  Cum: 34.1%
   [info] Score 24  Games: 43  Percent:  4.3%  Cum: 30.7%
   [info] Score 25  Games: 264  Percent: 26.4%  Cum: 26.4%
   [info] Average Score: 15.878
   [info] Average Utility: 44.956
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 98.9%
   [info] Score  3  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  4  Games: 15  Percent:  1.5%  Cum: 97.6%
   [info] Score  5  Games: 23  Percent:  2.3%  Cum: 96.1%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 93.8%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 92.0%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 90.0%
   [info] Score  9  Games: 33  Percent:  3.3%  Cum: 88.3%
   [info] Score 10  Games: 33  Percent:  3.3%  Cum: 85.0%
   [info] Score 11  Games: 42  Percent:  4.2%  Cum: 81.7%
   [info] Score 12  Games: 40  Percent:  4.0%  Cum: 77.5%
   [info] Score 13  Games: 46  Percent:  4.6%  Cum: 73.5%
   [info] Score 14  Games: 44  Percent:  4.4%  Cum: 68.9%
   [info] Score 15  Games: 39  Percent:  3.9%  Cum: 64.5%
   [info] Score 16  Games: 54  Percent:  5.4%  Cum: 60.6%
   [info] Score 17  Games: 61  Percent:  6.1%  Cum: 55.2%
   [info] Score 18  Games: 54  Percent:  5.4%  Cum: 49.1%
   [info] Score 19  Games: 54  Percent:  5.4%  Cum: 43.7%
   [info] Score 20  Games: 54  Percent:  5.4%  Cum: 38.3%
   [info] Score 21  Games: 63  Percent:  6.3%  Cum: 32.9%
   [info] Score 22  Games: 55  Percent:  5.5%  Cum: 26.6%
   [info] Score 23  Games: 77  Percent:  7.7%  Cum: 21.1%
   [info] Score 24  Games: 98  Percent:  9.8%  Cum: 13.4%
   [info] Score 25  Games: 36  Percent:  3.6%  Cum:  3.6%
   [info] Average Score: 16.504
   [info] Average Utility: 34.808
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.8%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 97.8%
   [info] Score  5  Games: 21  Percent:  2.1%  Cum: 96.8%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 94.7%
   [info] Score  7  Games: 27  Percent:  2.7%  Cum: 93.3%
   [info] Score  8  Games: 37  Percent:  3.7%  Cum: 90.6%
   [info] Score  9  Games: 52  Percent:  5.2%  Cum: 86.9%
   [info] Score 10  Games: 42  Percent:  4.2%  Cum: 81.7%
   [info] Score 11  Games: 54  Percent:  5.4%  Cum: 77.5%
   [info] Score 12  Games: 56  Percent:  5.6%  Cum: 72.1%
   [info] Score 13  Games: 61  Percent:  6.1%  Cum: 66.5%
   [info] Score 14  Games: 70  Percent:  7.0%  Cum: 60.4%
   [info] Score 15  Games: 68  Percent:  6.8%  Cum: 53.4%
   [info] Score 16  Games: 65  Percent:  6.5%  Cum: 46.6%
   [info] Score 17  Games: 64  Percent:  6.4%  Cum: 40.1%
   [info] Score 18  Games: 75  Percent:  7.5%  Cum: 33.7%
   [info] Score 19  Games: 54  Percent:  5.4%  Cum: 26.2%
   [info] Score 20  Games: 37  Percent:  3.7%  Cum: 20.8%
   [info] Score 21  Games: 43  Percent:  4.3%  Cum: 17.1%
   [info] Score 22  Games: 54  Percent:  5.4%  Cum: 12.8%
   [info] Score 23  Games: 48  Percent:  4.8%  Cum:  7.4%
   [info] Score 24  Games: 20  Percent:  2.0%  Cum:  2.6%
   [info] Score 25  Games:  6  Percent:  0.6%  Cum:  0.6%
   [info] Average Score: 14.776
   [info] Average Utility: 29.852
   [info]
   [info] Time: 872.869381644

   */

}
