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
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  5  Games: 14  Percent:  1.4%  Cum: 97.6%
   [info] Score  6  Games: 19  Percent:  1.9%  Cum: 96.2%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 94.3%
   [info] Score  8  Games: 19  Percent:  1.9%  Cum: 92.8%
   [info] Score  9  Games: 21  Percent:  2.1%  Cum: 90.9%
   [info] Score 10  Games: 27  Percent:  2.7%  Cum: 88.8%
   [info] Score 11  Games: 15  Percent:  1.5%  Cum: 86.1%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 84.6%
   [info] Score 13  Games:  8  Percent:  0.8%  Cum: 81.9%
   [info] Score 14  Games: 18  Percent:  1.8%  Cum: 81.1%
   [info] Score 15  Games: 18  Percent:  1.8%  Cum: 79.3%
   [info] Score 16  Games: 19  Percent:  1.9%  Cum: 77.5%
   [info] Score 17  Games: 19  Percent:  1.9%  Cum: 75.6%
   [info] Score 18  Games: 22  Percent:  2.2%  Cum: 73.7%
   [info] Score 19  Games: 18  Percent:  1.8%  Cum: 71.5%
   [info] Score 20  Games: 16  Percent:  1.6%  Cum: 69.7%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 68.1%
   [info] Score 22  Games: 26  Percent:  2.6%  Cum: 65.4%
   [info] Score 23  Games: 40  Percent:  4.0%  Cum: 62.8%
   [info] Score 24  Games: 67  Percent:  6.7%  Cum: 58.8%
   [info] Score 25  Games: 521  Percent: 52.1%  Cum: 52.1%
   [info] Average Score: 20.459
   [info] Average Utility: 66.968
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  4  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 99.5%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 99.0%
   [info] Score  7  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  8  Games: 14  Percent:  1.4%  Cum: 97.6%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 96.2%
   [info] Score 10  Games: 12  Percent:  1.2%  Cum: 94.8%
   [info] Score 11  Games: 13  Percent:  1.3%  Cum: 93.6%
   [info] Score 12  Games: 10  Percent:  1.0%  Cum: 92.3%
   [info] Score 13  Games: 22  Percent:  2.2%  Cum: 91.3%
   [info] Score 14  Games: 14  Percent:  1.4%  Cum: 89.1%
   [info] Score 15  Games: 17  Percent:  1.7%  Cum: 87.7%
   [info] Score 16  Games: 24  Percent:  2.4%  Cum: 86.0%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 83.6%
   [info] Score 18  Games: 21  Percent:  2.1%  Cum: 81.3%
   [info] Score 19  Games: 25  Percent:  2.5%  Cum: 79.2%
   [info] Score 20  Games: 43  Percent:  4.3%  Cum: 76.7%
   [info] Score 21  Games: 33  Percent:  3.3%  Cum: 72.4%
   [info] Score 22  Games: 71  Percent:  7.1%  Cum: 69.1%
   [info] Score 23  Games: 105  Percent: 10.5%  Cum: 62.0%
   [info] Score 24  Games: 145  Percent: 14.5%  Cum: 51.5%
   [info] Score 25  Games: 370  Percent: 37.0%  Cum: 37.0%
   [info] Average Score: 21.378
   [info] Average Utility: 61.256
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.6%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.3%
   [info] Score  6  Games:  6  Percent:  0.6%  Cum: 98.9%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 98.3%
   [info] Score  8  Games: 10  Percent:  1.0%  Cum: 97.3%
   [info] Score  9  Games: 18  Percent:  1.8%  Cum: 96.3%
   [info] Score 10  Games: 14  Percent:  1.4%  Cum: 94.5%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 93.1%
   [info] Score 12  Games: 18  Percent:  1.8%  Cum: 90.4%
   [info] Score 13  Games: 22  Percent:  2.2%  Cum: 88.6%
   [info] Score 14  Games: 28  Percent:  2.8%  Cum: 86.4%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 83.6%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 81.2%
   [info] Score 17  Games: 27  Percent:  2.7%  Cum: 78.3%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 75.6%
   [info] Score 19  Games: 48  Percent:  4.8%  Cum: 72.3%
   [info] Score 20  Games: 49  Percent:  4.9%  Cum: 67.5%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 62.6%
   [info] Score 22  Games: 101  Percent: 10.1%  Cum: 57.8%
   [info] Score 23  Games: 131  Percent: 13.1%  Cum: 47.7%
   [info] Score 24  Games: 191  Percent: 19.1%  Cum: 34.6%
   [info] Score 25  Games: 155  Percent: 15.5%  Cum: 15.5%
   [info] Average Score: 20.188
   [info] Average Utility: 48.126
   [info]
   [info] Time: 1662.130504277

   */

}
