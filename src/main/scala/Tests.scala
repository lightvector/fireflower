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
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 97.5%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 96.2%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 94.6%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 92.6%
   [info] Score  9  Games: 19  Percent:  1.9%  Cum: 90.9%
   [info] Score 10  Games: 25  Percent:  2.5%  Cum: 89.0%
   [info] Score 11  Games: 17  Percent:  1.7%  Cum: 86.5%
   [info] Score 12  Games: 28  Percent:  2.8%  Cum: 84.8%
   [info] Score 13  Games:  9  Percent:  0.9%  Cum: 82.0%
   [info] Score 14  Games: 16  Percent:  1.6%  Cum: 81.1%
   [info] Score 15  Games: 18  Percent:  1.8%  Cum: 79.5%
   [info] Score 16  Games: 16  Percent:  1.6%  Cum: 77.7%
   [info] Score 17  Games: 18  Percent:  1.8%  Cum: 76.1%
   [info] Score 18  Games: 24  Percent:  2.4%  Cum: 74.3%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 71.9%
   [info] Score 20  Games: 17  Percent:  1.7%  Cum: 69.8%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 68.1%
   [info] Score 22  Games: 30  Percent:  3.0%  Cum: 65.5%
   [info] Score 23  Games: 35  Percent:  3.5%  Cum: 62.5%
   [info] Score 24  Games: 72  Percent:  7.2%  Cum: 59.0%
   [info] Score 25  Games: 518  Percent: 51.8%  Cum: 51.8%
   [info] Average Score: 20.485
   [info] Average Utility: 66.87
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 99.4%
   [info] Score  6  Games:  8  Percent:  0.8%  Cum: 98.9%
   [info] Score  7  Games:  8  Percent:  0.8%  Cum: 98.1%
   [info] Score  8  Games: 11  Percent:  1.1%  Cum: 97.3%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 96.2%
   [info] Score 10  Games: 11  Percent:  1.1%  Cum: 94.6%
   [info] Score 11  Games:  9  Percent:  0.9%  Cum: 93.5%
   [info] Score 12  Games: 10  Percent:  1.0%  Cum: 92.6%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 91.6%
   [info] Score 14  Games: 17  Percent:  1.7%  Cum: 89.5%
   [info] Score 15  Games: 17  Percent:  1.7%  Cum: 87.8%
   [info] Score 16  Games: 23  Percent:  2.3%  Cum: 86.1%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 83.8%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 81.5%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 78.8%
   [info] Score 20  Games: 47  Percent:  4.7%  Cum: 75.7%
   [info] Score 21  Games: 50  Percent:  5.0%  Cum: 71.0%
   [info] Score 22  Games: 62  Percent:  6.2%  Cum: 66.0%
   [info] Score 23  Games: 85  Percent:  8.5%  Cum: 59.8%
   [info] Score 24  Games: 155  Percent: 15.5%  Cum: 51.3%
   [info] Score 25  Games: 358  Percent: 35.8%  Cum: 35.8%
   [info] Average Score: 21.289
   [info] Average Utility: 60.478
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
   [info] Score  8  Games:  9  Percent:  0.9%  Cum: 97.3%
   [info] Score  9  Games: 18  Percent:  1.8%  Cum: 96.4%
   [info] Score 10  Games: 16  Percent:  1.6%  Cum: 94.6%
   [info] Score 11  Games: 26  Percent:  2.6%  Cum: 93.0%
   [info] Score 12  Games: 17  Percent:  1.7%  Cum: 90.4%
   [info] Score 13  Games: 18  Percent:  1.8%  Cum: 88.7%
   [info] Score 14  Games: 26  Percent:  2.6%  Cum: 86.9%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 84.3%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 81.2%
   [info] Score 17  Games: 34  Percent:  3.4%  Cum: 78.3%
   [info] Score 18  Games: 41  Percent:  4.1%  Cum: 74.9%
   [info] Score 19  Games: 46  Percent:  4.6%  Cum: 70.8%
   [info] Score 20  Games: 41  Percent:  4.1%  Cum: 66.2%
   [info] Score 21  Games: 51  Percent:  5.1%  Cum: 62.1%
   [info] Score 22  Games: 91  Percent:  9.1%  Cum: 57.0%
   [info] Score 23  Games: 117  Percent: 11.7%  Cum: 47.9%
   [info] Score 24  Games: 211  Percent: 21.1%  Cum: 36.2%
   [info] Score 25  Games: 151  Percent: 15.1%  Cum: 15.1%
   [info] Average Score: 20.168
   [info] Average Utility: 47.886
   [info]
   [info] Time: 1625.137158675

   */

}
