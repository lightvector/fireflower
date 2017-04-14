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
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.7%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 99.1%
   [info] Score  4  Games: 16  Percent:  1.6%  Cum: 97.9%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 96.3%
   [info] Score  6  Games: 19  Percent:  1.9%  Cum: 94.1%
   [info] Score  7  Games: 30  Percent:  3.0%  Cum: 92.2%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 89.2%
   [info] Score  9  Games: 33  Percent:  3.3%  Cum: 86.4%
   [info] Score 10  Games: 33  Percent:  3.3%  Cum: 83.1%
   [info] Score 11  Games: 25  Percent:  2.5%  Cum: 79.8%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 77.3%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 74.4%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 72.1%
   [info] Score 15  Games: 30  Percent:  3.0%  Cum: 69.6%
   [info] Score 16  Games: 22  Percent:  2.2%  Cum: 66.6%
   [info] Score 17  Games: 24  Percent:  2.4%  Cum: 64.4%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 62.0%
   [info] Score 19  Games: 23  Percent:  2.3%  Cum: 59.3%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 57.0%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 54.8%
   [info] Score 22  Games: 27  Percent:  2.7%  Cum: 52.2%
   [info] Score 23  Games: 35  Percent:  3.5%  Cum: 49.5%
   [info] Score 24  Games: 82  Percent:  8.2%  Cum: 46.0%
   [info] Score 25  Games: 378  Percent: 37.8%  Cum: 37.8%
   [info] Average Score: 18.608
   [info] Average Utility: 56.116
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.7%
   [info] Score  3  Games:  5  Percent:  0.5%  Cum: 99.6%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.1%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 98.4%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 97.9%
   [info] Score  7  Games:  9  Percent:  0.9%  Cum: 97.4%
   [info] Score  8  Games:  8  Percent:  0.8%  Cum: 96.5%
   [info] Score  9  Games: 10  Percent:  1.0%  Cum: 95.7%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 94.7%
   [info] Score 11  Games: 20  Percent:  2.0%  Cum: 93.0%
   [info] Score 12  Games: 26  Percent:  2.6%  Cum: 91.0%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 88.4%
   [info] Score 14  Games: 17  Percent:  1.7%  Cum: 86.0%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 84.3%
   [info] Score 16  Games: 35  Percent:  3.5%  Cum: 82.2%
   [info] Score 17  Games: 29  Percent:  2.9%  Cum: 78.7%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 75.8%
   [info] Score 19  Games: 32  Percent:  3.2%  Cum: 72.5%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 69.3%
   [info] Score 21  Games: 67  Percent:  6.7%  Cum: 65.8%
   [info] Score 22  Games: 79  Percent:  7.9%  Cum: 59.1%
   [info] Score 23  Games: 73  Percent:  7.3%  Cum: 51.2%
   [info] Score 24  Games: 207  Percent: 20.7%  Cum: 43.9%
   [info] Score 25  Games: 232  Percent: 23.2%  Cum: 23.2%
   [info] Average Score: 20.433
   [info] Average Utility: 52.466
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.9%
   [info] Score  2  Games:  3  Percent:  0.3%  Cum: 99.6%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 99.3%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.5%
   [info] Score  5  Games:  9  Percent:  0.9%  Cum: 97.7%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 96.8%
   [info] Score  7  Games: 13  Percent:  1.3%  Cum: 96.1%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 94.8%
   [info] Score  9  Games: 13  Percent:  1.3%  Cum: 93.1%
   [info] Score 10  Games: 21  Percent:  2.1%  Cum: 91.8%
   [info] Score 11  Games: 26  Percent:  2.6%  Cum: 89.7%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 87.1%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 84.2%
   [info] Score 14  Games: 38  Percent:  3.8%  Cum: 81.9%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 78.1%
   [info] Score 16  Games: 49  Percent:  4.9%  Cum: 75.0%
   [info] Score 17  Games: 33  Percent:  3.3%  Cum: 70.1%
   [info] Score 18  Games: 60  Percent:  6.0%  Cum: 66.8%
   [info] Score 19  Games: 43  Percent:  4.3%  Cum: 60.8%
   [info] Score 20  Games: 63  Percent:  6.3%  Cum: 56.5%
   [info] Score 21  Games: 63  Percent:  6.3%  Cum: 50.2%
   [info] Score 22  Games: 86  Percent:  8.6%  Cum: 43.9%
   [info] Score 23  Games: 115  Percent: 11.5%  Cum: 35.3%
   [info] Score 24  Games: 171  Percent: 17.1%  Cum: 23.8%
   [info] Score 25  Games: 67  Percent:  6.7%  Cum:  6.7%
   [info] Average Score: 18.777
   [info] Average Utility: 40.904
   [info]
   [info] Time: 1105.643424237

   */

}
