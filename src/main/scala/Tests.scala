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

    runTests(prefix="",salt="g",numGames=numGames, numPlayers=numPlayers)
  }

  def runTests(prefix: String, salt: String, numGames: Int, numPlayers: Option[Int]): Unit = {
    val start = System.nanoTime()
    val rules2p = Rules.Standard(numPlayers=2,stopEarlyLoss=false)
    val rules3p = Rules.Standard(numPlayers=3,stopEarlyLoss=false)
    val rules4p = Rules.Standard(numPlayers=4,stopEarlyLoss=false)

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
    printScoreSummaryBombZero(rules2p,games2p)
    println("")
    println(name3p + ":")
    printScoreSummary(rules3p,games3p)
    printScoreSummaryBombZero(rules3p,games3p)
    println("")
    println(name4p + ":")
    printScoreSummary(rules4p,games4p)
    printScoreSummaryBombZero(rules4p,games4p)
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

  def printScoreSummaryBombZero(rules: Rules, games: List[Game]) = {
    val scoreTable = (0 to rules.maxScore).map { score =>
      (score,games.count(game => (if (game.numBombs > rules.maxBombs) 0 else game.numPlayed) == score))
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
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  4  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  5  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  6  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  7  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  8  Games:  1  Percent:  0.1%  Cum: 99.8%
   [info] Score  9  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score 10  Games:  2  Percent:  0.2%  Cum: 99.5%
   [info] Score 11  Games:  3  Percent:  0.3%  Cum: 99.3%
   [info] Score 12  Games:  1  Percent:  0.1%  Cum: 99.0%
   [info] Score 13  Games:  1  Percent:  0.1%  Cum: 98.9%
   [info] Score 14  Games:  4  Percent:  0.4%  Cum: 98.8%
   [info] Score 15  Games:  3  Percent:  0.3%  Cum: 98.4%
   [info] Score 16  Games:  7  Percent:  0.7%  Cum: 98.1%
   [info] Score 17  Games:  9  Percent:  0.9%  Cum: 97.4%
   [info] Score 18  Games: 15  Percent:  1.5%  Cum: 96.5%
   [info] Score 19  Games: 28  Percent:  2.8%  Cum: 95.0%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 92.2%
   [info] Score 21  Games: 38  Percent:  3.8%  Cum: 89.9%
   [info] Score 22  Games: 58  Percent:  5.8%  Cum: 86.1%
   [info] Score 23  Games: 114  Percent: 11.4%  Cum: 80.3%
   [info] Score 24  Games: 124  Percent: 12.4%  Cum: 68.9%
   [info] Score 25  Games: 565  Percent: 56.5%  Cum: 56.5%
   [info] Average Score: 23.537
   [info] Average Utility: 75.324

   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  5  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 99.4%
   [info] Score  7  Games:  1  Percent:  0.1%  Cum: 99.0%
   [info] Score  8  Games:  2  Percent:  0.2%  Cum: 98.9%
   [info] Score  9  Games:  1  Percent:  0.1%  Cum: 98.7%
   [info] Score 10  Games:  4  Percent:  0.4%  Cum: 98.6%
   [info] Score 11  Games:  8  Percent:  0.8%  Cum: 98.2%
   [info] Score 12  Games:  3  Percent:  0.3%  Cum: 97.4%
   [info] Score 13  Games:  3  Percent:  0.3%  Cum: 97.1%
   [info] Score 14  Games:  6  Percent:  0.6%  Cum: 96.8%
   [info] Score 15  Games:  9  Percent:  0.9%  Cum: 96.2%
   [info] Score 16  Games:  7  Percent:  0.7%  Cum: 95.3%
   [info] Score 17  Games:  9  Percent:  0.9%  Cum: 94.6%
   [info] Score 18  Games: 12  Percent:  1.2%  Cum: 93.7%
   [info] Score 19  Games: 24  Percent:  2.4%  Cum: 92.5%
   [info] Score 20  Games: 25  Percent:  2.5%  Cum: 90.1%
   [info] Score 21  Games: 42  Percent:  4.2%  Cum: 87.6%
   [info] Score 22  Games: 77  Percent:  7.7%  Cum: 83.4%
   [info] Score 23  Games: 135  Percent: 13.5%  Cum: 75.7%
   [info] Score 24  Games: 220  Percent: 22.0%  Cum: 62.2%
   [info] Score 25  Games: 402  Percent: 40.2%  Cum: 40.2%
   [info] Average Score: 22.953
   [info] Average Utility: 66.006

   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  4  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  5  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  6  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  7  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  8  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  9  Games:  1  Percent:  0.1%  Cum: 99.5%
   [info] Score 10  Games:  3  Percent:  0.3%  Cum: 99.4%
   [info] Score 11  Games:  2  Percent:  0.2%  Cum: 99.1%
   [info] Score 12  Games:  2  Percent:  0.2%  Cum: 98.9%
   [info] Score 13  Games:  6  Percent:  0.6%  Cum: 98.7%
   [info] Score 14  Games:  2  Percent:  0.2%  Cum: 98.1%
   [info] Score 15  Games:  9  Percent:  0.9%  Cum: 97.9%
   [info] Score 16  Games:  8  Percent:  0.8%  Cum: 97.0%
   [info] Score 17  Games:  9  Percent:  0.9%  Cum: 96.2%
   [info] Score 18  Games:  8  Percent:  0.8%  Cum: 95.3%
   [info] Score 19  Games: 25  Percent:  2.5%  Cum: 94.5%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 92.0%
   [info] Score 21  Games: 66  Percent:  6.6%  Cum: 88.5%
   [info] Score 22  Games: 124  Percent: 12.4%  Cum: 81.9%
   [info] Score 23  Games: 188  Percent: 18.8%  Cum: 69.5%
   [info] Score 24  Games: 242  Percent: 24.2%  Cum: 50.7%
   [info] Score 25  Games: 265  Percent: 26.5%  Cum: 26.5%
   [info] Average Score: 22.832
   [info] Average Utility: 58.914

   Much longer run:

   [info] HeuristicStandard2P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  3  Percent:  0.0%  Cum: 100.0%
   [info] Score  3  Games:  8  Percent:  0.0%  Cum: 100.0%
   [info] Score  4  Games: 11  Percent:  0.0%  Cum: 100.0%
   [info] Score  5  Games: 17  Percent:  0.1%  Cum: 99.9%
   [info] Score  6  Games: 18  Percent:  0.1%  Cum: 99.8%
   [info] Score  7  Games: 29  Percent:  0.1%  Cum: 99.8%
   [info] Score  8  Games: 26  Percent:  0.1%  Cum: 99.7%
   [info] Score  9  Games: 33  Percent:  0.1%  Cum: 99.6%
   [info] Score 10  Games: 37  Percent:  0.1%  Cum: 99.4%
   [info] Score 11  Games: 40  Percent:  0.2%  Cum: 99.3%
   [info] Score 12  Games: 46  Percent:  0.2%  Cum: 99.1%
   [info] Score 13  Games: 75  Percent:  0.3%  Cum: 98.9%
   [info] Score 14  Games: 79  Percent:  0.3%  Cum: 98.6%
   [info] Score 15  Games: 110  Percent:  0.4%  Cum: 98.3%
   [info] Score 16  Games: 165  Percent:  0.7%  Cum: 97.9%
   [info] Score 17  Games: 279  Percent:  1.1%  Cum: 97.2%
   [info] Score 18  Games: 378  Percent:  1.5%  Cum: 96.1%
   [info] Score 19  Games: 506  Percent:  2.0%  Cum: 94.6%
   [info] Score 20  Games: 825  Percent:  3.3%  Cum: 92.6%
   [info] Score 21  Games: 1377  Percent:  5.5%  Cum: 89.3%
   [info] Score 22  Games: 2091  Percent:  8.4%  Cum: 83.8%
   [info] Score 23  Games: 2514  Percent: 10.1%  Cum: 75.4%
   [info] Score 24  Games: 3181  Percent: 12.7%  Cum: 65.3%
   [info] Score 25  Games: 13152  Percent: 52.6%  Cum: 52.6%
   [info] Average Score: 23.37016
   [info] Average Utility: 73.04432
   [info] Score  0  Games: 1226  Percent:  4.9%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 95.1%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 95.1%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 95.1%
   [info] Score  4  Games:  0  Percent:  0.0%  Cum: 95.1%
   [info] Score  5  Games:  0  Percent:  0.0%  Cum: 95.1%
   [info] Score  6  Games:  0  Percent:  0.0%  Cum: 95.1%
   [info] Score  7  Games:  1  Percent:  0.0%  Cum: 95.1%
   [info] Score  8  Games:  1  Percent:  0.0%  Cum: 95.1%
   [info] Score  9  Games:  1  Percent:  0.0%  Cum: 95.1%
   [info] Score 10  Games:  4  Percent:  0.0%  Cum: 95.1%
   [info] Score 11  Games:  5  Percent:  0.0%  Cum: 95.1%
   [info] Score 12  Games:  6  Percent:  0.0%  Cum: 95.0%
   [info] Score 13  Games: 22  Percent:  0.1%  Cum: 95.0%
   [info] Score 14  Games: 27  Percent:  0.1%  Cum: 94.9%
   [info] Score 15  Games: 47  Percent:  0.2%  Cum: 94.8%
   [info] Score 16  Games: 89  Percent:  0.4%  Cum: 94.6%
   [info] Score 17  Games: 186  Percent:  0.7%  Cum: 94.3%
   [info] Score 18  Games: 270  Percent:  1.1%  Cum: 93.5%
   [info] Score 19  Games: 392  Percent:  1.6%  Cum: 92.5%
   [info] Score 20  Games: 722  Percent:  2.9%  Cum: 90.9%
   [info] Score 21  Games: 1242  Percent:  5.0%  Cum: 88.0%
   [info] Score 22  Games: 1978  Percent:  7.9%  Cum: 83.0%
   [info] Score 23  Games: 2474  Percent:  9.9%  Cum: 75.1%
   [info] Score 24  Games: 3155  Percent: 12.6%  Cum: 65.2%
   [info] Score 25  Games: 13152  Percent: 52.6%  Cum: 52.6%
   [info] Average Score: 22.55656
   [info] Average Utility: 71.41712

   */

}
