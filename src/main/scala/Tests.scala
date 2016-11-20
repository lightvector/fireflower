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
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  2  Games: 18  Percent:  1.8%  Cum: 99.3%
   [info] Score  3  Games: 23  Percent:  2.3%  Cum: 97.5%
   [info] Score  4  Games: 30  Percent:  3.0%  Cum: 95.2%
   [info] Score  5  Games: 34  Percent:  3.4%  Cum: 92.2%
   [info] Score  6  Games: 29  Percent:  2.9%  Cum: 88.8%
   [info] Score  7  Games: 27  Percent:  2.7%  Cum: 85.9%
   [info] Score  8  Games: 29  Percent:  2.9%  Cum: 83.2%
   [info] Score  9  Games: 32  Percent:  3.2%  Cum: 80.3%
   [info] Score 10  Games: 38  Percent:  3.8%  Cum: 77.1%
   [info] Score 11  Games: 37  Percent:  3.7%  Cum: 73.3%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 69.6%
   [info] Score 13  Games: 25  Percent:  2.5%  Cum: 67.2%
   [info] Score 14  Games: 28  Percent:  2.8%  Cum: 64.7%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 61.9%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 59.6%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 56.9%
   [info] Score 18  Games: 31  Percent:  3.1%  Cum: 54.6%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 51.5%
   [info] Score 20  Games: 21  Percent:  2.1%  Cum: 49.4%
   [info] Score 21  Games: 23  Percent:  2.3%  Cum: 47.3%
   [info] Score 22  Games: 28  Percent:  2.8%  Cum: 45.0%
   [info] Score 23  Games: 49  Percent:  4.9%  Cum: 42.2%
   [info] Score 24  Games: 75  Percent:  7.5%  Cum: 37.3%
   [info] Score 25  Games: 298  Percent: 29.8%  Cum: 29.8%
   [info] Average Score: 17.096
   [info] Average Utility: 49.092
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  3  Games: 14  Percent:  1.4%  Cum: 98.6%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 97.2%
   [info] Score  5  Games: 14  Percent:  1.4%  Cum: 95.8%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 94.4%
   [info] Score  7  Games: 18  Percent:  1.8%  Cum: 92.7%
   [info] Score  8  Games: 13  Percent:  1.3%  Cum: 90.9%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 89.6%
   [info] Score 10  Games: 26  Percent:  2.6%  Cum: 87.2%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 84.6%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 81.9%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 79.4%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 77.1%
   [info] Score 15  Games: 33  Percent:  3.3%  Cum: 73.8%
   [info] Score 16  Games: 42  Percent:  4.2%  Cum: 70.5%
   [info] Score 17  Games: 36  Percent:  3.6%  Cum: 66.3%
   [info] Score 18  Games: 43  Percent:  4.3%  Cum: 62.7%
   [info] Score 19  Games: 35  Percent:  3.5%  Cum: 58.4%
   [info] Score 20  Games: 40  Percent:  4.0%  Cum: 54.9%
   [info] Score 21  Games: 40  Percent:  4.0%  Cum: 50.9%
   [info] Score 22  Games: 57  Percent:  5.7%  Cum: 46.9%
   [info] Score 23  Games: 87  Percent:  8.7%  Cum: 41.2%
   [info] Score 24  Games: 203  Percent: 20.3%  Cum: 32.5%
   [info] Score 25  Games: 122  Percent: 12.2%  Cum: 12.2%
   [info] Average Score: 18.386
   [info] Average Utility: 42.872
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.2%
   [info] Score  3  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 97.6%
   [info] Score  5  Games: 18  Percent:  1.8%  Cum: 96.8%
   [info] Score  6  Games: 10  Percent:  1.0%  Cum: 95.0%
   [info] Score  7  Games: 17  Percent:  1.7%  Cum: 94.0%
   [info] Score  8  Games: 23  Percent:  2.3%  Cum: 92.3%
   [info] Score  9  Games: 25  Percent:  2.5%  Cum: 90.0%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 87.5%
   [info] Score 11  Games: 31  Percent:  3.1%  Cum: 85.7%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 82.6%
   [info] Score 13  Games: 47  Percent:  4.7%  Cum: 79.7%
   [info] Score 14  Games: 37  Percent:  3.7%  Cum: 75.0%
   [info] Score 15  Games: 50  Percent:  5.0%  Cum: 71.3%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 66.3%
   [info] Score 17  Games: 63  Percent:  6.3%  Cum: 62.5%
   [info] Score 18  Games: 55  Percent:  5.5%  Cum: 56.2%
   [info] Score 19  Games: 65  Percent:  6.5%  Cum: 50.7%
   [info] Score 20  Games: 63  Percent:  6.3%  Cum: 44.2%
   [info] Score 21  Games: 51  Percent:  5.1%  Cum: 37.9%
   [info] Score 22  Games: 72  Percent:  7.2%  Cum: 32.8%
   [info] Score 23  Games: 103  Percent: 10.3%  Cum: 25.6%
   [info] Score 24  Games: 127  Percent: 12.7%  Cum: 15.3%
   [info] Score 25  Games: 26  Percent:  2.6%  Cum:  2.6%
   [info] Average Score: 17.388
   [info] Average Utility: 36.076
   [info]
   [info] Time: 988.592612504

   */

}
