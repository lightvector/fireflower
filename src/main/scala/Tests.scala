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
   [info] Score  0  Games:  4  Percent:  0.4%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  2  Games: 16  Percent:  1.6%  Cum: 99.0%
   [info] Score  3  Games: 26  Percent:  2.6%  Cum: 97.4%
   [info] Score  4  Games: 33  Percent:  3.3%  Cum: 94.8%
   [info] Score  5  Games: 40  Percent:  4.0%  Cum: 91.5%
   [info] Score  6  Games: 34  Percent:  3.4%  Cum: 87.5%
   [info] Score  7  Games: 42  Percent:  4.2%  Cum: 84.1%
   [info] Score  8  Games: 35  Percent:  3.5%  Cum: 79.9%
   [info] Score  9  Games: 29  Percent:  2.9%  Cum: 76.4%
   [info] Score 10  Games: 35  Percent:  3.5%  Cum: 73.5%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 70.0%
   [info] Score 12  Games: 40  Percent:  4.0%  Cum: 67.3%
   [info] Score 13  Games: 31  Percent:  3.1%  Cum: 63.3%
   [info] Score 14  Games: 37  Percent:  3.7%  Cum: 60.2%
   [info] Score 15  Games: 26  Percent:  2.6%  Cum: 56.5%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 53.9%
   [info] Score 17  Games: 37  Percent:  3.7%  Cum: 51.4%
   [info] Score 18  Games: 17  Percent:  1.7%  Cum: 47.7%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 46.0%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 43.9%
   [info] Score 21  Games: 40  Percent:  4.0%  Cum: 41.6%
   [info] Score 22  Games: 29  Percent:  2.9%  Cum: 37.6%
   [info] Score 23  Games: 23  Percent:  2.3%  Cum: 34.7%
   [info] Score 24  Games: 53  Percent:  5.3%  Cum: 32.4%
   [info] Score 25  Games: 271  Percent: 27.1%  Cum: 27.1%
   [info] Average Score: 16.173
   [info] Average Utility: 45.896
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games: 11  Percent:  1.1%  Cum: 99.8%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 98.7%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.2%
   [info] Score  4  Games: 15  Percent:  1.5%  Cum: 96.6%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 95.1%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 93.8%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 92.1%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 89.8%
   [info] Score  9  Games: 19  Percent:  1.9%  Cum: 88.0%
   [info] Score 10  Games: 32  Percent:  3.2%  Cum: 86.1%
   [info] Score 11  Games: 24  Percent:  2.4%  Cum: 82.9%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 80.5%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 77.6%
   [info] Score 14  Games: 36  Percent:  3.6%  Cum: 74.6%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 71.0%
   [info] Score 16  Games: 35  Percent:  3.5%  Cum: 68.1%
   [info] Score 17  Games: 27  Percent:  2.7%  Cum: 64.6%
   [info] Score 18  Games: 41  Percent:  4.1%  Cum: 61.9%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 57.8%
   [info] Score 20  Games: 45  Percent:  4.5%  Cum: 54.7%
   [info] Score 21  Games: 42  Percent:  4.2%  Cum: 50.2%
   [info] Score 22  Games: 42  Percent:  4.2%  Cum: 46.0%
   [info] Score 23  Games: 73  Percent:  7.3%  Cum: 41.8%
   [info] Score 24  Games: 217  Percent: 21.7%  Cum: 34.5%
   [info] Score 25  Games: 128  Percent: 12.8%  Cum: 12.8%
   [info] Average Score: 18.172
   [info] Average Utility: 42.744
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.8%
   [info] Score  2  Games: 11  Percent:  1.1%  Cum: 99.2%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.1%
   [info] Score  4  Games: 11  Percent:  1.1%  Cum: 96.5%
   [info] Score  5  Games: 23  Percent:  2.3%  Cum: 95.4%
   [info] Score  6  Games:  8  Percent:  0.8%  Cum: 93.1%
   [info] Score  7  Games: 18  Percent:  1.8%  Cum: 92.3%
   [info] Score  8  Games: 20  Percent:  2.0%  Cum: 90.5%
   [info] Score  9  Games: 23  Percent:  2.3%  Cum: 88.5%
   [info] Score 10  Games: 25  Percent:  2.5%  Cum: 86.2%
   [info] Score 11  Games: 33  Percent:  3.3%  Cum: 83.7%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 80.4%
   [info] Score 13  Games: 39  Percent:  3.9%  Cum: 77.7%
   [info] Score 14  Games: 35  Percent:  3.5%  Cum: 73.8%
   [info] Score 15  Games: 41  Percent:  4.1%  Cum: 70.3%
   [info] Score 16  Games: 41  Percent:  4.1%  Cum: 66.2%
   [info] Score 17  Games: 45  Percent:  4.5%  Cum: 62.1%
   [info] Score 18  Games: 52  Percent:  5.2%  Cum: 57.6%
   [info] Score 19  Games: 60  Percent:  6.0%  Cum: 52.4%
   [info] Score 20  Games: 50  Percent:  5.0%  Cum: 46.4%
   [info] Score 21  Games: 44  Percent:  4.4%  Cum: 41.4%
   [info] Score 22  Games: 66  Percent:  6.6%  Cum: 37.0%
   [info] Score 23  Games: 118  Percent: 11.8%  Cum: 30.4%
   [info] Score 24  Games: 159  Percent: 15.9%  Cum: 18.6%
   [info] Score 25  Games: 27  Percent:  2.7%  Cum:  2.7%
   [info] Average Score: 17.403
   [info] Average Utility: 36.156
   [info]
   [info] Time: 943.316033634

   */

}
