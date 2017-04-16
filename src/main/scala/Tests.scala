package fireflower

object PlayerTests {

  def main(args: Array[String]): Unit = {
    val numGames = {
      if(args.length >= 1)
        args(0).toInt
      else
        1000
    }
    val numPlayers = {
      if(args.length >= 2)
        Some(args(1).toInt)
      else
        None
    }
    println("NumGames=" + numGames)

    runTests(prefix="",salt="c",numGames=numGames, numPlayers=numPlayers)
  }

  def runTests(prefix: String, salt: String, numGames: Int, numPlayers: Option[Int]): Unit = {
    val start = System.nanoTime()
    val rules2p = Rules.Standard(numPlayers=2)
    val rules3p = Rules.Standard(numPlayers=3)
    val rules4p = Rules.Standard(numPlayers=4)

    def makeRunSeed(name:String): Long = {
      RandUtils.sha256Long(RandUtils.sha256Long(name) + salt)
    }

    val name2p = prefix + "HeuristicStandard2P"
    val games2p = {
      Sim.runMulti(
        name = name2p,
        rules = rules2p,
        numGames,
        runSeed = makeRunSeed(name2p),
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
        runSeed = makeRunSeed(name3p),
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
        runSeed = makeRunSeed(name4p),
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
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 100.0%
   [info] Score  3  Games: 13  Percent:  1.3%  Cum: 99.6%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 97.5%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 95.6%
   [info] Score  7  Games: 19  Percent:  1.9%  Cum: 94.0%
   [info] Score  8  Games: 31  Percent:  3.1%  Cum: 92.1%
   [info] Score  9  Games: 23  Percent:  2.3%  Cum: 89.0%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 86.7%
   [info] Score 11  Games: 17  Percent:  1.7%  Cum: 84.7%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 83.0%
   [info] Score 13  Games: 33  Percent:  3.3%  Cum: 81.1%
   [info] Score 14  Games: 26  Percent:  2.6%  Cum: 77.8%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 75.2%
   [info] Score 16  Games: 28  Percent:  2.8%  Cum: 72.8%
   [info] Score 17  Games: 17  Percent:  1.7%  Cum: 70.0%
   [info] Score 18  Games: 28  Percent:  2.8%  Cum: 68.3%
   [info] Score 19  Games: 33  Percent:  3.3%  Cum: 65.5%
   [info] Score 20  Games: 26  Percent:  2.6%  Cum: 62.2%
   [info] Score 21  Games: 33  Percent:  3.3%  Cum: 59.6%
   [info] Score 22  Games: 14  Percent:  1.4%  Cum: 56.3%
   [info] Score 23  Games: 31  Percent:  3.1%  Cum: 54.9%
   [info] Score 24  Games: 84  Percent:  8.4%  Cum: 51.8%
   [info] Score 25  Games: 434  Percent: 43.4%  Cum: 43.4%
   [info] Average Score: 19.594
   [info] Average Utility: 60.888
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.6%
   [info] Score  4  Games:  2  Percent:  0.2%  Cum: 99.4%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 99.2%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.5%
   [info] Score  7  Games:  7  Percent:  0.7%  Cum: 98.1%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 97.4%
   [info] Score  9  Games: 15  Percent:  1.5%  Cum: 95.7%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 94.2%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 92.4%
   [info] Score 12  Games: 18  Percent:  1.8%  Cum: 90.3%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 88.5%
   [info] Score 14  Games: 23  Percent:  2.3%  Cum: 86.4%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 84.1%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 82.0%
   [info] Score 17  Games: 30  Percent:  3.0%  Cum: 79.3%
   [info] Score 18  Games: 38  Percent:  3.8%  Cum: 76.3%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 72.5%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 69.5%
   [info] Score 21  Games: 47  Percent:  4.7%  Cum: 66.0%
   [info] Score 22  Games: 66  Percent:  6.6%  Cum: 61.3%
   [info] Score 23  Games: 72  Percent:  7.2%  Cum: 54.7%
   [info] Score 24  Games: 170  Percent: 17.0%  Cum: 47.5%
   [info] Score 25  Games: 305  Percent: 30.5%  Cum: 30.5%
   [info] Average Score: 20.63
   [info] Average Utility: 56.51
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.7%
   [info] Score  3  Games:  1  Percent:  0.1%  Cum: 99.6%
   [info] Score  4  Games:  4  Percent:  0.4%  Cum: 99.5%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 99.1%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 97.6%
   [info] Score  8  Games:  9  Percent:  0.9%  Cum: 96.6%
   [info] Score  9  Games: 21  Percent:  2.1%  Cum: 95.7%
   [info] Score 10  Games: 26  Percent:  2.6%  Cum: 93.6%
   [info] Score 11  Games: 36  Percent:  3.6%  Cum: 91.0%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 87.4%
   [info] Score 13  Games: 18  Percent:  1.8%  Cum: 84.7%
   [info] Score 14  Games: 30  Percent:  3.0%  Cum: 82.9%
   [info] Score 15  Games: 34  Percent:  3.4%  Cum: 79.9%
   [info] Score 16  Games: 32  Percent:  3.2%  Cum: 76.5%
   [info] Score 17  Games: 41  Percent:  4.1%  Cum: 73.3%
   [info] Score 18  Games: 45  Percent:  4.5%  Cum: 69.2%
   [info] Score 19  Games: 49  Percent:  4.9%  Cum: 64.7%
   [info] Score 20  Games: 49  Percent:  4.9%  Cum: 59.8%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 54.9%
   [info] Score 22  Games: 86  Percent:  8.6%  Cum: 50.1%
   [info] Score 23  Games: 109  Percent: 10.9%  Cum: 41.5%
   [info] Score 24  Games: 183  Percent: 18.3%  Cum: 30.6%
   [info] Score 25  Games: 123  Percent: 12.3%  Cum: 12.3%
   [info] Average Score: 19.385
   [info] Average Utility: 44.92
   [info]
   [info] Time: 1245.568210635

   */

}
