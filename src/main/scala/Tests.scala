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

    runTests(prefix="",salt="d",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.7%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 99.1%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 98.1%
   [info] Score  5  Games: 24  Percent:  2.4%  Cum: 97.1%
   [info] Score  6  Games: 15  Percent:  1.5%  Cum: 94.7%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 93.2%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 91.2%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 89.4%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 88.0%
   [info] Score 11  Games: 12  Percent:  1.2%  Cum: 86.0%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 84.8%
   [info] Score 13  Games: 18  Percent:  1.8%  Cum: 82.9%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 81.1%
   [info] Score 15  Games: 10  Percent:  1.0%  Cum: 78.9%
   [info] Score 16  Games: 26  Percent:  2.6%  Cum: 77.9%
   [info] Score 17  Games: 17  Percent:  1.7%  Cum: 75.3%
   [info] Score 18  Games: 21  Percent:  2.1%  Cum: 73.6%
   [info] Score 19  Games: 20  Percent:  2.0%  Cum: 71.5%
   [info] Score 20  Games: 14  Percent:  1.4%  Cum: 69.5%
   [info] Score 21  Games: 25  Percent:  2.5%  Cum: 68.1%
   [info] Score 22  Games: 35  Percent:  3.5%  Cum: 65.6%
   [info] Score 23  Games: 43  Percent:  4.3%  Cum: 62.1%
   [info] Score 24  Games: 82  Percent:  8.2%  Cum: 57.8%
   [info] Score 25  Games: 496  Percent: 49.6%  Cum: 49.6%
   [info] Average Score: 20.352
   [info] Average Utility: 65.504
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.6%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.3%
   [info] Score  6  Games: 13  Percent:  1.3%  Cum: 98.9%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 97.6%
   [info] Score  8  Games: 10  Percent:  1.0%  Cum: 96.6%
   [info] Score  9  Games: 15  Percent:  1.5%  Cum: 95.6%
   [info] Score 10  Games:  9  Percent:  0.9%  Cum: 94.1%
   [info] Score 11  Games: 14  Percent:  1.4%  Cum: 93.2%
   [info] Score 12  Games: 23  Percent:  2.3%  Cum: 91.8%
   [info] Score 13  Games: 12  Percent:  1.2%  Cum: 89.5%
   [info] Score 14  Games: 21  Percent:  2.1%  Cum: 88.3%
   [info] Score 15  Games: 14  Percent:  1.4%  Cum: 86.2%
   [info] Score 16  Games: 24  Percent:  2.4%  Cum: 84.8%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 82.4%
   [info] Score 18  Games: 20  Percent:  2.0%  Cum: 80.1%
   [info] Score 19  Games: 36  Percent:  3.6%  Cum: 78.1%
   [info] Score 20  Games: 37  Percent:  3.7%  Cum: 74.5%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 70.8%
   [info] Score 22  Games: 69  Percent:  6.9%  Cum: 68.1%
   [info] Score 23  Games: 83  Percent:  8.3%  Cum: 61.2%
   [info] Score 24  Games: 167  Percent: 16.7%  Cum: 52.9%
   [info] Score 25  Games: 362  Percent: 36.2%  Cum: 36.2%
   [info] Average Score: 21.193
   [info] Average Utility: 60.486
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.3%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.9%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 98.2%
   [info] Score  8  Games: 11  Percent:  1.1%  Cum: 97.2%
   [info] Score  9  Games: 20  Percent:  2.0%  Cum: 96.1%
   [info] Score 10  Games: 16  Percent:  1.6%  Cum: 94.1%
   [info] Score 11  Games: 25  Percent:  2.5%  Cum: 92.5%
   [info] Score 12  Games: 15  Percent:  1.5%  Cum: 90.0%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 88.5%
   [info] Score 14  Games: 24  Percent:  2.4%  Cum: 86.4%
   [info] Score 15  Games: 27  Percent:  2.7%  Cum: 84.0%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 81.3%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 78.2%
   [info] Score 18  Games: 39  Percent:  3.9%  Cum: 75.7%
   [info] Score 19  Games: 37  Percent:  3.7%  Cum: 71.8%
   [info] Score 20  Games: 49  Percent:  4.9%  Cum: 68.1%
   [info] Score 21  Games: 56  Percent:  5.6%  Cum: 63.2%
   [info] Score 22  Games: 102  Percent: 10.2%  Cum: 57.6%
   [info] Score 23  Games: 127  Percent: 12.7%  Cum: 47.4%
   [info] Score 24  Games: 197  Percent: 19.7%  Cum: 34.7%
   [info] Score 25  Games: 150  Percent: 15.0%  Cum: 15.0%
   [info] Average Score: 20.178
   [info] Average Utility: 47.856
   [info]
   [info] Time: 1557.160296493

   */

}
