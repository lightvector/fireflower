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

    runTests(prefix="",salt="b",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  3  Games: 11  Percent:  1.1%  Cum: 99.3%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 98.2%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 96.8%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 94.6%
   [info] Score  7  Games: 28  Percent:  2.8%  Cum: 93.0%
   [info] Score  8  Games: 31  Percent:  3.1%  Cum: 90.2%
   [info] Score  9  Games: 31  Percent:  3.1%  Cum: 87.1%
   [info] Score 10  Games: 33  Percent:  3.3%  Cum: 84.0%
   [info] Score 11  Games: 25  Percent:  2.5%  Cum: 80.7%
   [info] Score 12  Games: 28  Percent:  2.8%  Cum: 78.2%
   [info] Score 13  Games: 25  Percent:  2.5%  Cum: 75.4%
   [info] Score 14  Games: 26  Percent:  2.6%  Cum: 72.9%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 70.3%
   [info] Score 16  Games: 22  Percent:  2.2%  Cum: 67.4%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 65.2%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 62.7%
   [info] Score 19  Games: 25  Percent:  2.5%  Cum: 60.0%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 57.5%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 55.3%
   [info] Score 22  Games: 25  Percent:  2.5%  Cum: 52.7%
   [info] Score 23  Games: 35  Percent:  3.5%  Cum: 50.2%
   [info] Score 24  Games: 84  Percent:  8.4%  Cum: 46.7%
   [info] Score 25  Games: 383  Percent: 38.3%  Cum: 38.3%
   [info] Average Score: 18.765
   [info] Average Utility: 56.68
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  5  Games:  3  Percent:  0.3%  Cum: 99.2%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.9%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 98.4%
   [info] Score  8  Games:  7  Percent:  0.7%  Cum: 97.4%
   [info] Score  9  Games: 10  Percent:  1.0%  Cum: 96.7%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 95.7%
   [info] Score 11  Games: 20  Percent:  2.0%  Cum: 94.0%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 92.0%
   [info] Score 13  Games: 28  Percent:  2.8%  Cum: 89.3%
   [info] Score 14  Games: 20  Percent:  2.0%  Cum: 86.5%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 84.5%
   [info] Score 16  Games: 36  Percent:  3.6%  Cum: 82.4%
   [info] Score 17  Games: 31  Percent:  3.1%  Cum: 78.8%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 75.7%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 72.4%
   [info] Score 20  Games: 36  Percent:  3.6%  Cum: 69.4%
   [info] Score 21  Games: 67  Percent:  6.7%  Cum: 65.8%
   [info] Score 22  Games: 80  Percent:  8.0%  Cum: 59.1%
   [info] Score 23  Games: 75  Percent:  7.5%  Cum: 51.1%
   [info] Score 24  Games: 201  Percent: 20.1%  Cum: 43.6%
   [info] Score 25  Games: 235  Percent: 23.5%  Cum: 23.5%
   [info] Average Score: 20.54
   [info] Average Utility: 52.83
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.7%
   [info] Score  3  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.0%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 97.5%
   [info] Score  7  Games:  8  Percent:  0.8%  Cum: 96.8%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 96.0%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 94.5%
   [info] Score 10  Games: 21  Percent:  2.1%  Cum: 93.1%
   [info] Score 11  Games: 31  Percent:  3.1%  Cum: 91.0%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 87.9%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 85.0%
   [info] Score 14  Games: 37  Percent:  3.7%  Cum: 82.7%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 79.0%
   [info] Score 16  Games: 49  Percent:  4.9%  Cum: 76.1%
   [info] Score 17  Games: 36  Percent:  3.6%  Cum: 71.2%
   [info] Score 18  Games: 59  Percent:  5.9%  Cum: 67.6%
   [info] Score 19  Games: 47  Percent:  4.7%  Cum: 61.7%
   [info] Score 20  Games: 64  Percent:  6.4%  Cum: 57.0%
   [info] Score 21  Games: 65  Percent:  6.5%  Cum: 50.6%
   [info] Score 22  Games: 85  Percent:  8.5%  Cum: 44.1%
   [info] Score 23  Games: 115  Percent: 11.5%  Cum: 35.6%
   [info] Score 24  Games: 173  Percent: 17.3%  Cum: 24.1%
   [info] Score 25  Games: 68  Percent:  6.8%  Cum:  6.8%
   [info] Average Score: 18.949
   [info] Average Utility: 41.298
   [info]
   [info] Time: 1134.173316722
   */

}
