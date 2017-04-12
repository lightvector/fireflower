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
   [info] Score  3  Games: 15  Percent:  1.5%  Cum: 98.2%
   [info] Score  4  Games: 17  Percent:  1.7%  Cum: 96.7%
   [info] Score  5  Games: 24  Percent:  2.4%  Cum: 95.0%
   [info] Score  6  Games: 24  Percent:  2.4%  Cum: 92.6%
   [info] Score  7  Games: 29  Percent:  2.9%  Cum: 90.2%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 87.3%
   [info] Score  9  Games: 34  Percent:  3.4%  Cum: 84.5%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 81.1%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 79.4%
   [info] Score 12  Games: 26  Percent:  2.6%  Cum: 76.6%
   [info] Score 13  Games: 16  Percent:  1.6%  Cum: 74.0%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 72.4%
   [info] Score 15  Games: 27  Percent:  2.7%  Cum: 69.3%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 66.6%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 63.9%
   [info] Score 18  Games: 29  Percent:  2.9%  Cum: 61.6%
   [info] Score 19  Games: 20  Percent:  2.0%  Cum: 58.7%
   [info] Score 20  Games: 28  Percent:  2.8%  Cum: 56.7%
   [info] Score 21  Games: 31  Percent:  3.1%  Cum: 53.9%
   [info] Score 22  Games: 25  Percent:  2.5%  Cum: 50.8%
   [info] Score 23  Games: 39  Percent:  3.9%  Cum: 48.3%
   [info] Score 24  Games: 77  Percent:  7.7%  Cum: 44.4%
   [info] Score 25  Games: 367  Percent: 36.7%  Cum: 36.7%
   [info] Average Score: 18.382
   [info] Average Utility: 55.114
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.6%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 97.4%
   [info] Score  5  Games: 12  Percent:  1.2%  Cum: 96.0%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 94.8%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 93.0%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 91.5%
   [info] Score  9  Games: 18  Percent:  1.8%  Cum: 90.0%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 88.2%
   [info] Score 11  Games: 26  Percent:  2.6%  Cum: 85.8%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 83.2%
   [info] Score 13  Games: 22  Percent:  2.2%  Cum: 81.3%
   [info] Score 14  Games: 26  Percent:  2.6%  Cum: 79.1%
   [info] Score 15  Games: 34  Percent:  3.4%  Cum: 76.5%
   [info] Score 16  Games: 30  Percent:  3.0%  Cum: 73.1%
   [info] Score 17  Games: 33  Percent:  3.3%  Cum: 70.1%
   [info] Score 18  Games: 29  Percent:  2.9%  Cum: 66.8%
   [info] Score 19  Games: 35  Percent:  3.5%  Cum: 63.9%
   [info] Score 20  Games: 30  Percent:  3.0%  Cum: 60.4%
   [info] Score 21  Games: 29  Percent:  2.9%  Cum: 57.4%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 54.5%
   [info] Score 23  Games: 65  Percent:  6.5%  Cum: 47.8%
   [info] Score 24  Games: 202  Percent: 20.2%  Cum: 41.3%
   [info] Score 25  Games: 211  Percent: 21.1%  Cum: 21.1%
   [info] Average Score: 19.107
   [info] Average Utility: 48.764
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.9%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.5%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.6%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 97.4%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 96.4%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 94.8%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 93.7%
   [info] Score  8  Games: 21  Percent:  2.1%  Cum: 91.4%
   [info] Score  9  Games: 18  Percent:  1.8%  Cum: 89.3%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 87.5%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 85.3%
   [info] Score 12  Games: 35  Percent:  3.5%  Cum: 83.0%
   [info] Score 13  Games: 37  Percent:  3.7%  Cum: 79.5%
   [info] Score 14  Games: 30  Percent:  3.0%  Cum: 75.8%
   [info] Score 15  Games: 35  Percent:  3.5%  Cum: 72.8%
   [info] Score 16  Games: 33  Percent:  3.3%  Cum: 69.3%
   [info] Score 17  Games: 50  Percent:  5.0%  Cum: 66.0%
   [info] Score 18  Games: 45  Percent:  4.5%  Cum: 61.0%
   [info] Score 19  Games: 48  Percent:  4.8%  Cum: 56.5%
   [info] Score 20  Games: 52  Percent:  5.2%  Cum: 51.7%
   [info] Score 21  Games: 53  Percent:  5.3%  Cum: 46.5%
   [info] Score 22  Games: 80  Percent:  8.0%  Cum: 41.2%
   [info] Score 23  Games: 108  Percent: 10.8%  Cum: 33.2%
   [info] Score 24  Games: 161  Percent: 16.1%  Cum: 22.4%
   [info] Score 25  Games: 63  Percent:  6.3%  Cum:  6.3%
   [info] Average Score: 17.99
   [info] Average Utility: 39.13
   [info]
   [info] Time: 971.510955262
      */

}
