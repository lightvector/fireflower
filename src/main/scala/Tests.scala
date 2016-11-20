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
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games: 19  Percent:  1.9%  Cum: 99.2%
   [info] Score  3  Games: 24  Percent:  2.4%  Cum: 97.3%
   [info] Score  4  Games: 26  Percent:  2.6%  Cum: 94.9%
   [info] Score  5  Games: 30  Percent:  3.0%  Cum: 92.3%
   [info] Score  6  Games: 30  Percent:  3.0%  Cum: 89.3%
   [info] Score  7  Games: 29  Percent:  2.9%  Cum: 86.3%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 83.4%
   [info] Score  9  Games: 29  Percent:  2.9%  Cum: 80.6%
   [info] Score 10  Games: 31  Percent:  3.1%  Cum: 77.7%
   [info] Score 11  Games: 31  Percent:  3.1%  Cum: 74.6%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 71.5%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 68.6%
   [info] Score 14  Games: 27  Percent:  2.7%  Cum: 66.2%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 63.5%
   [info] Score 16  Games: 18  Percent:  1.8%  Cum: 61.3%
   [info] Score 17  Games: 31  Percent:  3.1%  Cum: 59.5%
   [info] Score 18  Games: 30  Percent:  3.0%  Cum: 56.4%
   [info] Score 19  Games: 23  Percent:  2.3%  Cum: 53.4%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 51.1%
   [info] Score 21  Games: 31  Percent:  3.1%  Cum: 48.8%
   [info] Score 22  Games: 32  Percent:  3.2%  Cum: 45.7%
   [info] Score 23  Games: 57  Percent:  5.7%  Cum: 42.5%
   [info] Score 24  Games: 65  Percent:  6.5%  Cum: 36.8%
   [info] Score 25  Games: 303  Percent: 30.3%  Cum: 30.3%
   [info] Average Score: 17.309
   [info] Average Utility: 49.768
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.6%
   [info] Score  4  Games: 13  Percent:  1.3%  Cum: 97.4%
   [info] Score  5  Games: 14  Percent:  1.4%  Cum: 96.1%
   [info] Score  6  Games: 19  Percent:  1.9%  Cum: 94.7%
   [info] Score  7  Games: 18  Percent:  1.8%  Cum: 92.8%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 91.0%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 89.5%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 87.1%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 84.7%
   [info] Score 12  Games: 21  Percent:  2.1%  Cum: 81.9%
   [info] Score 13  Games: 19  Percent:  1.9%  Cum: 79.8%
   [info] Score 14  Games: 27  Percent:  2.7%  Cum: 77.9%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 75.2%
   [info] Score 16  Games: 44  Percent:  4.4%  Cum: 72.3%
   [info] Score 17  Games: 29  Percent:  2.9%  Cum: 67.9%
   [info] Score 18  Games: 43  Percent:  4.3%  Cum: 65.0%
   [info] Score 19  Games: 41  Percent:  4.1%  Cum: 60.7%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 56.6%
   [info] Score 21  Games: 45  Percent:  4.5%  Cum: 53.1%
   [info] Score 22  Games: 57  Percent:  5.7%  Cum: 48.6%
   [info] Score 23  Games: 89  Percent:  8.9%  Cum: 42.9%
   [info] Score 24  Games: 202  Percent: 20.2%  Cum: 34.0%
   [info] Score 25  Games: 138  Percent: 13.8%  Cum: 13.8%
   [info] Average Score: 18.605
   [info] Average Utility: 44.11
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.2%
   [info] Score  3  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 97.6%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 96.8%
   [info] Score  6  Games: 12  Percent:  1.2%  Cum: 95.2%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 94.0%
   [info] Score  8  Games: 21  Percent:  2.1%  Cum: 92.0%
   [info] Score  9  Games: 20  Percent:  2.0%  Cum: 89.9%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 87.9%
   [info] Score 11  Games: 26  Percent:  2.6%  Cum: 85.5%
   [info] Score 12  Games: 28  Percent:  2.8%  Cum: 82.9%
   [info] Score 13  Games: 47  Percent:  4.7%  Cum: 80.1%
   [info] Score 14  Games: 37  Percent:  3.7%  Cum: 75.4%
   [info] Score 15  Games: 42  Percent:  4.2%  Cum: 71.7%
   [info] Score 16  Games: 46  Percent:  4.6%  Cum: 67.5%
   [info] Score 17  Games: 56  Percent:  5.6%  Cum: 62.9%
   [info] Score 18  Games: 58  Percent:  5.8%  Cum: 57.3%
   [info] Score 19  Games: 59  Percent:  5.9%  Cum: 51.5%
   [info] Score 20  Games: 50  Percent:  5.0%  Cum: 45.6%
   [info] Score 21  Games: 46  Percent:  4.6%  Cum: 40.6%
   [info] Score 22  Games: 76  Percent:  7.6%  Cum: 36.0%
   [info] Score 23  Games: 112  Percent: 11.2%  Cum: 28.4%
   [info] Score 24  Games: 142  Percent: 14.2%  Cum: 17.2%
   [info] Score 25  Games: 30  Percent:  3.0%  Cum:  3.0%
   [info] Average Score: 17.562
   [info] Average Utility: 36.624
   [info]
   [info] Time: 1097.321477563

   */

}
