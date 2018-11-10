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

    runTests(prefix="",salt="e",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  8  Percent:  0.8%  Cum: 99.8%
   [info] Score  3  Games:  7  Percent:  0.7%  Cum: 99.0%
   [info] Score  4  Games:  9  Percent:  0.9%  Cum: 98.3%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 97.4%
   [info] Score  6  Games: 12  Percent:  1.2%  Cum: 96.1%
   [info] Score  7  Games: 17  Percent:  1.7%  Cum: 94.9%
   [info] Score  8  Games: 24  Percent:  2.4%  Cum: 93.2%
   [info] Score  9  Games: 18  Percent:  1.8%  Cum: 90.8%
   [info] Score 10  Games: 27  Percent:  2.7%  Cum: 89.0%
   [info] Score 11  Games: 18  Percent:  1.8%  Cum: 86.3%
   [info] Score 12  Games: 16  Percent:  1.6%  Cum: 84.5%
   [info] Score 13  Games:  7  Percent:  0.7%  Cum: 82.9%
   [info] Score 14  Games: 16  Percent:  1.6%  Cum: 82.2%
   [info] Score 15  Games: 14  Percent:  1.4%  Cum: 80.6%
   [info] Score 16  Games: 14  Percent:  1.4%  Cum: 79.2%
   [info] Score 17  Games: 16  Percent:  1.6%  Cum: 77.8%
   [info] Score 18  Games: 19  Percent:  1.9%  Cum: 76.2%
   [info] Score 19  Games: 15  Percent:  1.5%  Cum: 74.3%
   [info] Score 20  Games: 18  Percent:  1.8%  Cum: 72.8%
   [info] Score 21  Games: 29  Percent:  2.9%  Cum: 71.0%
   [info] Score 22  Games: 29  Percent:  2.9%  Cum: 68.1%
   [info] Score 23  Games: 39  Percent:  3.9%  Cum: 65.2%
   [info] Score 24  Games: 69  Percent:  6.9%  Cum: 61.3%
   [info] Score 25  Games: 544  Percent: 54.4%  Cum: 54.4%
   [info] Average Score: 20.753
   [info] Average Utility: 68.706
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.9%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.6%
   [info] Score  5  Games: 10  Percent:  1.0%  Cum: 99.3%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  7  Games: 11  Percent:  1.1%  Cum: 97.6%
   [info] Score  8  Games: 14  Percent:  1.4%  Cum: 96.5%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 95.1%
   [info] Score 10  Games: 14  Percent:  1.4%  Cum: 93.9%
   [info] Score 11  Games: 19  Percent:  1.9%  Cum: 92.5%
   [info] Score 12  Games: 18  Percent:  1.8%  Cum: 90.6%
   [info] Score 13  Games: 27  Percent:  2.7%  Cum: 88.8%
   [info] Score 14  Games: 11  Percent:  1.1%  Cum: 86.1%
   [info] Score 15  Games: 17  Percent:  1.7%  Cum: 85.0%
   [info] Score 16  Games: 15  Percent:  1.5%  Cum: 83.3%
   [info] Score 17  Games: 20  Percent:  2.0%  Cum: 81.8%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 79.8%
   [info] Score 19  Games: 23  Percent:  2.3%  Cum: 77.1%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 74.8%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 72.5%
   [info] Score 22  Games: 46  Percent:  4.6%  Cum: 69.9%
   [info] Score 23  Games: 65  Percent:  6.5%  Cum: 65.3%
   [info] Score 24  Games: 153  Percent: 15.3%  Cum: 58.8%
   [info] Score 25  Games: 435  Percent: 43.5%  Cum: 43.5%
   [info] Average Score: 21.3
   [info] Average Utility: 64.35
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 98.8%
   [info] Score  6  Games: 12  Percent:  1.2%  Cum: 98.0%
   [info] Score  7  Games:  9  Percent:  0.9%  Cum: 96.8%
   [info] Score  8  Games: 10  Percent:  1.0%  Cum: 95.9%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 94.9%
   [info] Score 10  Games: 16  Percent:  1.6%  Cum: 93.7%
   [info] Score 11  Games: 24  Percent:  2.4%  Cum: 92.1%
   [info] Score 12  Games: 20  Percent:  2.0%  Cum: 89.7%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 87.7%
   [info] Score 14  Games: 18  Percent:  1.8%  Cum: 84.7%
   [info] Score 15  Games: 25  Percent:  2.5%  Cum: 82.9%
   [info] Score 16  Games: 21  Percent:  2.1%  Cum: 80.4%
   [info] Score 17  Games: 17  Percent:  1.7%  Cum: 78.3%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 76.6%
   [info] Score 19  Games: 26  Percent:  2.6%  Cum: 74.0%
   [info] Score 20  Games: 27  Percent:  2.7%  Cum: 71.4%
   [info] Score 21  Games: 50  Percent:  5.0%  Cum: 68.7%
   [info] Score 22  Games: 70  Percent:  7.0%  Cum: 63.7%
   [info] Score 23  Games: 113  Percent: 11.3%  Cum: 56.7%
   [info] Score 24  Games: 204  Percent: 20.4%  Cum: 45.4%
   [info] Score 25  Games: 250  Percent: 25.0%  Cum: 25.0%
   [info] Average Score: 20.543
   [info] Average Utility: 53.586
   [info]
   [info] Time: 1699.798215862

   */

}
