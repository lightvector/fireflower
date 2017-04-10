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
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.8%
   [info] Score  2  Games: 13  Percent:  1.3%  Cum: 99.4%
   [info] Score  3  Games: 17  Percent:  1.7%  Cum: 98.1%
   [info] Score  4  Games: 26  Percent:  2.6%  Cum: 96.4%
   [info] Score  5  Games: 32  Percent:  3.2%  Cum: 93.8%
   [info] Score  6  Games: 28  Percent:  2.8%  Cum: 90.6%
   [info] Score  7  Games: 32  Percent:  3.2%  Cum: 87.8%
   [info] Score  8  Games: 30  Percent:  3.0%  Cum: 84.6%
   [info] Score  9  Games: 26  Percent:  2.6%  Cum: 81.6%
   [info] Score 10  Games: 27  Percent:  2.7%  Cum: 79.0%
   [info] Score 11  Games: 26  Percent:  2.6%  Cum: 76.3%
   [info] Score 12  Games: 35  Percent:  3.5%  Cum: 73.7%
   [info] Score 13  Games: 19  Percent:  1.9%  Cum: 70.2%
   [info] Score 14  Games: 30  Percent:  3.0%  Cum: 68.3%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 65.3%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 62.9%
   [info] Score 17  Games: 30  Percent:  3.0%  Cum: 60.4%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 57.4%
   [info] Score 19  Games: 24  Percent:  2.4%  Cum: 54.8%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 52.4%
   [info] Score 21  Games: 31  Percent:  3.1%  Cum: 50.2%
   [info] Score 22  Games: 15  Percent:  1.5%  Cum: 47.1%
   [info] Score 23  Games: 43  Percent:  4.3%  Cum: 45.6%
   [info] Score 24  Games: 83  Percent:  8.3%  Cum: 41.3%
   [info] Score 25  Games: 330  Percent: 33.0%  Cum: 33.0%
   [info] Average Score: 17.7
   [info] Average Utility: 51.9
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.6%
   [info] Score  4  Games: 13  Percent:  1.3%  Cum: 97.4%
   [info] Score  5  Games: 14  Percent:  1.4%  Cum: 96.1%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 94.7%
   [info] Score  7  Games: 18  Percent:  1.8%  Cum: 92.9%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 91.1%
   [info] Score  9  Games: 22  Percent:  2.2%  Cum: 89.4%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 87.2%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 84.8%
   [info] Score 12  Games: 20  Percent:  2.0%  Cum: 82.0%
   [info] Score 13  Games: 19  Percent:  1.9%  Cum: 80.0%
   [info] Score 14  Games: 24  Percent:  2.4%  Cum: 78.1%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 75.7%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 73.4%
   [info] Score 17  Games: 29  Percent:  2.9%  Cum: 70.3%
   [info] Score 18  Games: 38  Percent:  3.8%  Cum: 67.4%
   [info] Score 19  Games: 32  Percent:  3.2%  Cum: 63.6%
   [info] Score 20  Games: 31  Percent:  3.1%  Cum: 60.4%
   [info] Score 21  Games: 39  Percent:  3.9%  Cum: 57.3%
   [info] Score 22  Games: 69  Percent:  6.9%  Cum: 53.4%
   [info] Score 23  Games: 72  Percent:  7.2%  Cum: 46.5%
   [info] Score 24  Games: 205  Percent: 20.5%  Cum: 39.3%
   [info] Score 25  Games: 188  Percent: 18.8%  Cum: 18.8%
   [info] Average Score: 18.973
   [info] Average Utility: 47.346
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.2%
   [info] Score  3  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 97.6%
   [info] Score  5  Games: 15  Percent:  1.5%  Cum: 97.0%
   [info] Score  6  Games: 10  Percent:  1.0%  Cum: 95.5%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 94.5%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 92.3%
   [info] Score  9  Games: 17  Percent:  1.7%  Cum: 90.5%
   [info] Score 10  Games: 21  Percent:  2.1%  Cum: 88.8%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 86.7%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 83.9%
   [info] Score 13  Games: 42  Percent:  4.2%  Cum: 81.4%
   [info] Score 14  Games: 40  Percent:  4.0%  Cum: 77.2%
   [info] Score 15  Games: 39  Percent:  3.9%  Cum: 73.2%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 69.3%
   [info] Score 17  Games: 45  Percent:  4.5%  Cum: 65.5%
   [info] Score 18  Games: 56  Percent:  5.6%  Cum: 61.0%
   [info] Score 19  Games: 62  Percent:  6.2%  Cum: 55.4%
   [info] Score 20  Games: 48  Percent:  4.8%  Cum: 49.2%
   [info] Score 21  Games: 50  Percent:  5.0%  Cum: 44.4%
   [info] Score 22  Games: 81  Percent:  8.1%  Cum: 39.4%
   [info] Score 23  Games: 106  Percent: 10.6%  Cum: 31.3%
   [info] Score 24  Games: 152  Percent: 15.2%  Cum: 20.7%
   [info] Score 25  Games: 55  Percent:  5.5%  Cum:  5.5%
   [info] Average Score: 17.975
   [info] Average Utility: 38.7
   [info]
   [info] Time: 1030.955938153

   */

}
