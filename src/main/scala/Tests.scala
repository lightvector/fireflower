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
   [info] Score  6  Games:  9  Percent:  0.9%  Cum: 98.3%
   [info] Score  7  Games: 11  Percent:  1.1%  Cum: 97.4%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 96.3%
   [info] Score  9  Games: 13  Percent:  1.3%  Cum: 94.8%
   [info] Score 10  Games: 14  Percent:  1.4%  Cum: 93.5%
   [info] Score 11  Games: 19  Percent:  1.9%  Cum: 92.1%
   [info] Score 12  Games: 17  Percent:  1.7%  Cum: 90.2%
   [info] Score 13  Games: 29  Percent:  2.9%  Cum: 88.5%
   [info] Score 14  Games: 12  Percent:  1.2%  Cum: 85.6%
   [info] Score 15  Games: 17  Percent:  1.7%  Cum: 84.4%
   [info] Score 16  Games: 15  Percent:  1.5%  Cum: 82.7%
   [info] Score 17  Games: 19  Percent:  1.9%  Cum: 81.2%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 79.3%
   [info] Score 19  Games: 25  Percent:  2.5%  Cum: 76.7%
   [info] Score 20  Games: 26  Percent:  2.6%  Cum: 74.2%
   [info] Score 21  Games: 24  Percent:  2.4%  Cum: 71.6%
   [info] Score 22  Games: 47  Percent:  4.7%  Cum: 69.2%
   [info] Score 23  Games: 66  Percent:  6.6%  Cum: 64.5%
   [info] Score 24  Games: 148  Percent: 14.8%  Cum: 57.9%
   [info] Score 25  Games: 431  Percent: 43.1%  Cum: 43.1%
   [info] Average Score: 21.203
   [info] Average Utility: 63.956
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 99.0%
   [info] Score  6  Games: 12  Percent:  1.2%  Cum: 98.2%
   [info] Score  7  Games:  9  Percent:  0.9%  Cum: 97.0%
   [info] Score  8  Games:  9  Percent:  0.9%  Cum: 96.1%
   [info] Score  9  Games: 13  Percent:  1.3%  Cum: 95.2%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 93.9%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 92.2%
   [info] Score 12  Games: 20  Percent:  2.0%  Cum: 90.0%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 88.0%
   [info] Score 14  Games: 18  Percent:  1.8%  Cum: 85.0%
   [info] Score 15  Games: 25  Percent:  2.5%  Cum: 83.2%
   [info] Score 16  Games: 17  Percent:  1.7%  Cum: 80.7%
   [info] Score 17  Games: 19  Percent:  1.9%  Cum: 79.0%
   [info] Score 18  Games: 25  Percent:  2.5%  Cum: 77.1%
   [info] Score 19  Games: 27  Percent:  2.7%  Cum: 74.6%
   [info] Score 20  Games: 31  Percent:  3.1%  Cum: 71.9%
   [info] Score 21  Games: 45  Percent:  4.5%  Cum: 68.8%
   [info] Score 22  Games: 65  Percent:  6.5%  Cum: 64.3%
   [info] Score 23  Games: 122  Percent: 12.2%  Cum: 57.8%
   [info] Score 24  Games: 212  Percent: 21.2%  Cum: 45.6%
   [info] Score 25  Games: 244  Percent: 24.4%  Cum: 24.4%
   [info] Average Score: 20.61
   [info] Average Utility: 53.42
   [info]
   [info] Time: 1671.72100677

   */

}
