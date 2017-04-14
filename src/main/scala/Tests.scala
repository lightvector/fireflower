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
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.9%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 99.5%
   [info] Score  4  Games: 21  Percent:  2.1%  Cum: 98.5%
   [info] Score  5  Games: 24  Percent:  2.4%  Cum: 96.4%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 94.0%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 92.2%
   [info] Score  8  Games: 31  Percent:  3.1%  Cum: 89.8%
   [info] Score  9  Games: 34  Percent:  3.4%  Cum: 86.7%
   [info] Score 10  Games: 44  Percent:  4.4%  Cum: 83.3%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 78.9%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 76.1%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 73.6%
   [info] Score 14  Games: 20  Percent:  2.0%  Cum: 71.3%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 69.3%
   [info] Score 16  Games: 23  Percent:  2.3%  Cum: 67.1%
   [info] Score 17  Games: 29  Percent:  2.9%  Cum: 64.8%
   [info] Score 18  Games: 25  Percent:  2.5%  Cum: 61.9%
   [info] Score 19  Games: 28  Percent:  2.8%  Cum: 59.4%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 56.6%
   [info] Score 21  Games: 23  Percent:  2.3%  Cum: 54.4%
   [info] Score 22  Games: 27  Percent:  2.7%  Cum: 52.1%
   [info] Score 23  Games: 35  Percent:  3.5%  Cum: 49.4%
   [info] Score 24  Games: 86  Percent:  8.6%  Cum: 45.9%
   [info] Score 25  Games: 373  Percent: 37.3%  Cum: 37.3%
   [info] Average Score: 18.584
   [info] Average Utility: 55.818
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.6%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.1%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.7%
   [info] Score  7  Games:  6  Percent:  0.6%  Cum: 98.3%
   [info] Score  8  Games:  8  Percent:  0.8%  Cum: 97.7%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 96.9%
   [info] Score 10  Games: 16  Percent:  1.6%  Cum: 95.5%
   [info] Score 11  Games: 16  Percent:  1.6%  Cum: 93.9%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 92.3%
   [info] Score 13  Games: 26  Percent:  2.6%  Cum: 89.6%
   [info] Score 14  Games: 26  Percent:  2.6%  Cum: 87.0%
   [info] Score 15  Games: 20  Percent:  2.0%  Cum: 84.4%
   [info] Score 16  Games: 24  Percent:  2.4%  Cum: 82.4%
   [info] Score 17  Games: 35  Percent:  3.5%  Cum: 80.0%
   [info] Score 18  Games: 30  Percent:  3.0%  Cum: 76.5%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 73.5%
   [info] Score 20  Games: 38  Percent:  3.8%  Cum: 70.5%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 66.7%
   [info] Score 22  Games: 86  Percent:  8.6%  Cum: 62.1%
   [info] Score 23  Games: 87  Percent:  8.7%  Cum: 53.5%
   [info] Score 24  Games: 201  Percent: 20.1%  Cum: 44.8%
   [info] Score 25  Games: 247  Percent: 24.7%  Cum: 24.7%
   [info] Average Score: 20.675
   [info] Average Utility: 53.7
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 98.8%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.2%
   [info] Score  7  Games: 12  Percent:  1.2%  Cum: 97.5%
   [info] Score  8  Games: 13  Percent:  1.3%  Cum: 96.3%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 95.0%
   [info] Score 10  Games: 25  Percent:  2.5%  Cum: 93.4%
   [info] Score 11  Games: 19  Percent:  1.9%  Cum: 90.9%
   [info] Score 12  Games: 31  Percent:  3.1%  Cum: 89.0%
   [info] Score 13  Games: 26  Percent:  2.6%  Cum: 85.9%
   [info] Score 14  Games: 30  Percent:  3.0%  Cum: 83.3%
   [info] Score 15  Games: 30  Percent:  3.0%  Cum: 80.3%
   [info] Score 16  Games: 37  Percent:  3.7%  Cum: 77.3%
   [info] Score 17  Games: 39  Percent:  3.9%  Cum: 73.6%
   [info] Score 18  Games: 60  Percent:  6.0%  Cum: 69.7%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 63.7%
   [info] Score 20  Games: 54  Percent:  5.4%  Cum: 58.7%
   [info] Score 21  Games: 63  Percent:  6.3%  Cum: 53.3%
   [info] Score 22  Games: 84  Percent:  8.4%  Cum: 47.0%
   [info] Score 23  Games: 133  Percent: 13.3%  Cum: 38.6%
   [info] Score 24  Games: 178  Percent: 17.8%  Cum: 25.3%
   [info] Score 25  Games: 75  Percent:  7.5%  Cum:  7.5%
   [info] Average Score: 19.225
   [info] Average Utility: 42.2
   [info]
   [info] Time: 1099.962799798

   */

}
