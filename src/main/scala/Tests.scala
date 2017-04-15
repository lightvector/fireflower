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
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 99.5%
   [info] Score  4  Games: 18  Percent:  1.8%  Cum: 98.7%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 96.9%
   [info] Score  6  Games: 21  Percent:  2.1%  Cum: 95.3%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 93.2%
   [info] Score  8  Games: 26  Percent:  2.6%  Cum: 90.9%
   [info] Score  9  Games: 27  Percent:  2.7%  Cum: 88.3%
   [info] Score 10  Games: 29  Percent:  2.9%  Cum: 85.6%
   [info] Score 11  Games: 35  Percent:  3.5%  Cum: 82.7%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 79.2%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 76.7%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 73.7%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 71.5%
   [info] Score 16  Games: 17  Percent:  1.7%  Cum: 69.2%
   [info] Score 17  Games: 24  Percent:  2.4%  Cum: 67.5%
   [info] Score 18  Games: 30  Percent:  3.0%  Cum: 65.1%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 62.1%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 59.1%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 56.9%
   [info] Score 22  Games: 27  Percent:  2.7%  Cum: 54.2%
   [info] Score 23  Games: 40  Percent:  4.0%  Cum: 51.5%
   [info] Score 24  Games: 77  Percent:  7.7%  Cum: 47.5%
   [info] Score 25  Games: 398  Percent: 39.8%  Cum: 39.8%
   [info] Average Score: 19.05
   [info] Average Utility: 58.0
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 99.0%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.5%
   [info] Score  7  Games:  7  Percent:  0.7%  Cum: 97.8%
   [info] Score  8  Games:  6  Percent:  0.6%  Cum: 97.1%
   [info] Score  9  Games:  7  Percent:  0.7%  Cum: 96.5%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 95.8%
   [info] Score 11  Games: 18  Percent:  1.8%  Cum: 94.0%
   [info] Score 12  Games: 22  Percent:  2.2%  Cum: 92.2%
   [info] Score 13  Games: 26  Percent:  2.6%  Cum: 90.0%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 87.4%
   [info] Score 15  Games: 15  Percent:  1.5%  Cum: 84.9%
   [info] Score 16  Games: 21  Percent:  2.1%  Cum: 83.4%
   [info] Score 17  Games: 31  Percent:  3.1%  Cum: 81.3%
   [info] Score 18  Games: 31  Percent:  3.1%  Cum: 78.2%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 75.1%
   [info] Score 20  Games: 39  Percent:  3.9%  Cum: 72.0%
   [info] Score 21  Games: 42  Percent:  4.2%  Cum: 68.1%
   [info] Score 22  Games: 75  Percent:  7.5%  Cum: 63.9%
   [info] Score 23  Games: 79  Percent:  7.9%  Cum: 56.4%
   [info] Score 24  Games: 228  Percent: 22.8%  Cum: 48.5%
   [info] Score 25  Games: 257  Percent: 25.7%  Cum: 25.7%
   [info] Average Score: 20.852
   [info] Average Utility: 54.554
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.8%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  4  Games:  4  Percent:  0.4%  Cum: 99.4%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 99.0%
   [info] Score  6  Games:  3  Percent:  0.3%  Cum: 98.3%
   [info] Score  7  Games: 11  Percent:  1.1%  Cum: 98.0%
   [info] Score  8  Games: 12  Percent:  1.2%  Cum: 96.9%
   [info] Score  9  Games: 11  Percent:  1.1%  Cum: 95.7%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 94.6%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 92.8%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 90.5%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 88.0%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 85.9%
   [info] Score 15  Games: 33  Percent:  3.3%  Cum: 82.6%
   [info] Score 16  Games: 39  Percent:  3.9%  Cum: 79.3%
   [info] Score 17  Games: 42  Percent:  4.2%  Cum: 75.4%
   [info] Score 18  Games: 52  Percent:  5.2%  Cum: 71.2%
   [info] Score 19  Games: 52  Percent:  5.2%  Cum: 66.0%
   [info] Score 20  Games: 50  Percent:  5.0%  Cum: 60.8%
   [info] Score 21  Games: 69  Percent:  6.9%  Cum: 55.8%
   [info] Score 22  Games: 98  Percent:  9.8%  Cum: 48.9%
   [info] Score 23  Games: 138  Percent: 13.8%  Cum: 39.1%
   [info] Score 24  Games: 170  Percent: 17.0%  Cum: 25.3%
   [info] Score 25  Games: 83  Percent:  8.3%  Cum:  8.3%
   [info] Average Score: 19.513
   [info] Average Utility: 43.176
   [info]
   [info] Time: 1118.212873617

   */

}
