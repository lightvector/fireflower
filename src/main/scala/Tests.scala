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
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.8%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.4%
   [info] Score  3  Games: 21  Percent:  2.1%  Cum: 98.5%
   [info] Score  4  Games: 23  Percent:  2.3%  Cum: 96.4%
   [info] Score  5  Games: 25  Percent:  2.5%  Cum: 94.1%
   [info] Score  6  Games: 22  Percent:  2.2%  Cum: 91.6%
   [info] Score  7  Games: 31  Percent:  3.1%  Cum: 89.4%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 86.3%
   [info] Score  9  Games: 34  Percent:  3.4%  Cum: 83.5%
   [info] Score 10  Games: 34  Percent:  3.4%  Cum: 80.1%
   [info] Score 11  Games: 30  Percent:  3.0%  Cum: 76.7%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 73.7%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 71.0%
   [info] Score 14  Games: 28  Percent:  2.8%  Cum: 68.6%
   [info] Score 15  Games: 30  Percent:  3.0%  Cum: 65.8%
   [info] Score 16  Games: 19  Percent:  1.9%  Cum: 62.8%
   [info] Score 17  Games: 24  Percent:  2.4%  Cum: 60.9%
   [info] Score 18  Games: 22  Percent:  2.2%  Cum: 58.5%
   [info] Score 19  Games: 22  Percent:  2.2%  Cum: 56.3%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 54.1%
   [info] Score 21  Games: 24  Percent:  2.4%  Cum: 52.2%
   [info] Score 22  Games: 24  Percent:  2.4%  Cum: 49.8%
   [info] Score 23  Games: 31  Percent:  3.1%  Cum: 47.4%
   [info] Score 24  Games: 81  Percent:  8.1%  Cum: 44.3%
   [info] Score 25  Games: 362  Percent: 36.2%  Cum: 36.2%
   [info] Average Score: 17.974
   [info] Average Utility: 54.048
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.5%
   [info] Score  3  Games: 13  Percent:  1.3%  Cum: 99.3%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 98.0%
   [info] Score  5  Games: 10  Percent:  1.0%  Cum: 96.6%
   [info] Score  6  Games:  6  Percent:  0.6%  Cum: 95.6%
   [info] Score  7  Games: 19  Percent:  1.9%  Cum: 95.0%
   [info] Score  8  Games: 11  Percent:  1.1%  Cum: 93.1%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 92.0%
   [info] Score 10  Games: 16  Percent:  1.6%  Cum: 90.8%
   [info] Score 11  Games: 18  Percent:  1.8%  Cum: 89.2%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 87.4%
   [info] Score 13  Games: 19  Percent:  1.9%  Cum: 84.5%
   [info] Score 14  Games: 18  Percent:  1.8%  Cum: 82.6%
   [info] Score 15  Games: 20  Percent:  2.0%  Cum: 80.8%
   [info] Score 16  Games: 36  Percent:  3.6%  Cum: 78.8%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 75.2%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 72.7%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 70.1%
   [info] Score 20  Games: 36  Percent:  3.6%  Cum: 67.0%
   [info] Score 21  Games: 63  Percent:  6.3%  Cum: 63.4%
   [info] Score 22  Games: 79  Percent:  7.9%  Cum: 57.1%
   [info] Score 23  Games: 68  Percent:  6.8%  Cum: 49.2%
   [info] Score 24  Games: 203  Percent: 20.3%  Cum: 42.4%
   [info] Score 25  Games: 221  Percent: 22.1%  Cum: 22.1%
   [info] Average Score: 19.822
   [info] Average Utility: 50.694
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.4%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.5%
   [info] Score  4  Games: 16  Percent:  1.6%  Cum: 97.5%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 95.9%
   [info] Score  6  Games: 10  Percent:  1.0%  Cum: 94.6%
   [info] Score  7  Games: 12  Percent:  1.2%  Cum: 93.6%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 92.4%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 90.6%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 89.0%
   [info] Score 11  Games: 24  Percent:  2.4%  Cum: 86.6%
   [info] Score 12  Games: 26  Percent:  2.6%  Cum: 84.2%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 81.6%
   [info] Score 14  Games: 35  Percent:  3.5%  Cum: 79.2%
   [info] Score 15  Games: 30  Percent:  3.0%  Cum: 75.7%
   [info] Score 16  Games: 45  Percent:  4.5%  Cum: 72.7%
   [info] Score 17  Games: 32  Percent:  3.2%  Cum: 68.2%
   [info] Score 18  Games: 57  Percent:  5.7%  Cum: 65.0%
   [info] Score 19  Games: 46  Percent:  4.6%  Cum: 59.3%
   [info] Score 20  Games: 63  Percent:  6.3%  Cum: 54.7%
   [info] Score 21  Games: 56  Percent:  5.6%  Cum: 48.4%
   [info] Score 22  Games: 84  Percent:  8.4%  Cum: 42.8%
   [info] Score 23  Games: 111  Percent: 11.1%  Cum: 34.4%
   [info] Score 24  Games: 169  Percent: 16.9%  Cum: 23.3%
   [info] Score 25  Games: 64  Percent:  6.4%  Cum:  6.4%
   [info] Average Score: 18.337
   [info] Average Utility: 39.874
   [info]
   [info] Time: 1110.555978734
   */

}
