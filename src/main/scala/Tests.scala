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

    runTests(prefix="",salt="a",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  2  Games: 13  Percent:  1.3%  Cum: 99.5%
   [info] Score  3  Games: 18  Percent:  1.8%  Cum: 98.2%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 96.4%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 95.2%
   [info] Score  6  Games: 26  Percent:  2.6%  Cum: 93.0%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 90.4%
   [info] Score  8  Games: 23  Percent:  2.3%  Cum: 88.2%
   [info] Score  9  Games: 28  Percent:  2.8%  Cum: 85.9%
   [info] Score 10  Games: 21  Percent:  2.1%  Cum: 83.1%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 81.0%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 78.3%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 75.9%
   [info] Score 14  Games: 29  Percent:  2.9%  Cum: 73.5%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 70.6%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 67.7%
   [info] Score 17  Games: 24  Percent:  2.4%  Cum: 65.0%
   [info] Score 18  Games: 32  Percent:  3.2%  Cum: 62.6%
   [info] Score 19  Games: 18  Percent:  1.8%  Cum: 59.4%
   [info] Score 20  Games: 28  Percent:  2.8%  Cum: 57.6%
   [info] Score 21  Games: 24  Percent:  2.4%  Cum: 54.8%
   [info] Score 22  Games: 28  Percent:  2.8%  Cum: 52.4%
   [info] Score 23  Games: 45  Percent:  4.5%  Cum: 49.6%
   [info] Score 24  Games: 76  Percent:  7.6%  Cum: 45.1%
   [info] Score 25  Games: 375  Percent: 37.5%  Cum: 37.5%
   [info] Average Score: 18.607
   [info] Average Utility: 55.964
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.2%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 98.8%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 98.0%
   [info] Score  5  Games:  9  Percent:  0.9%  Cum: 97.0%
   [info] Score  6  Games: 12  Percent:  1.2%  Cum: 96.1%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 94.9%
   [info] Score  8  Games: 13  Percent:  1.3%  Cum: 93.4%
   [info] Score  9  Games: 13  Percent:  1.3%  Cum: 92.1%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 90.8%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 89.1%
   [info] Score 12  Games: 13  Percent:  1.3%  Cum: 87.0%
   [info] Score 13  Games: 17  Percent:  1.7%  Cum: 85.7%
   [info] Score 14  Games: 28  Percent:  2.8%  Cum: 84.0%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 81.2%
   [info] Score 16  Games: 32  Percent:  3.2%  Cum: 79.1%
   [info] Score 17  Games: 28  Percent:  2.8%  Cum: 75.9%
   [info] Score 18  Games: 37  Percent:  3.7%  Cum: 73.1%
   [info] Score 19  Games: 44  Percent:  4.4%  Cum: 69.4%
   [info] Score 20  Games: 36  Percent:  3.6%  Cum: 65.0%
   [info] Score 21  Games: 41  Percent:  4.1%  Cum: 61.4%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 57.3%
   [info] Score 23  Games: 65  Percent:  6.5%  Cum: 50.6%
   [info] Score 24  Games: 223  Percent: 22.3%  Cum: 44.1%
   [info] Score 25  Games: 218  Percent: 21.8%  Cum: 21.8%
   [info] Average Score: 19.847
   [info] Average Utility: 50.594
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.5%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.9%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 97.9%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 97.2%
   [info] Score  6  Games:  9  Percent:  0.9%  Cum: 95.9%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 95.0%
   [info] Score  8  Games: 22  Percent:  2.2%  Cum: 92.6%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 90.4%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 89.2%
   [info] Score 11  Games: 17  Percent:  1.7%  Cum: 87.0%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 85.3%
   [info] Score 13  Games: 33  Percent:  3.3%  Cum: 82.6%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 79.3%
   [info] Score 15  Games: 38  Percent:  3.8%  Cum: 77.1%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 73.3%
   [info] Score 17  Games: 51  Percent:  5.1%  Cum: 70.2%
   [info] Score 18  Games: 51  Percent:  5.1%  Cum: 65.1%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 60.0%
   [info] Score 20  Games: 52  Percent:  5.2%  Cum: 55.0%
   [info] Score 21  Games: 66  Percent:  6.6%  Cum: 49.8%
   [info] Score 22  Games: 90  Percent:  9.0%  Cum: 43.2%
   [info] Score 23  Games: 116  Percent: 11.6%  Cum: 34.2%
   [info] Score 24  Games: 168  Percent: 16.8%  Cum: 22.6%
   [info] Score 25  Games: 58  Percent:  5.8%  Cum:  5.8%
   [info] Average Score: 18.469
   [info] Average Utility: 39.838
   [info]
   [info] Time: 988.634434466
   */

}
