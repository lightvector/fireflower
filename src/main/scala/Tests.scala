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

    runTests(prefix="",salt="c",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 100.0%
   [info] Score  3  Games: 13  Percent:  1.3%  Cum: 99.6%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 97.5%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 95.5%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 93.8%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 91.8%
   [info] Score  9  Games: 25  Percent:  2.5%  Cum: 89.0%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 86.5%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 84.5%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 82.2%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 79.7%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 76.7%
   [info] Score 15  Games: 18  Percent:  1.8%  Cum: 74.2%
   [info] Score 16  Games: 26  Percent:  2.6%  Cum: 72.4%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 69.8%
   [info] Score 18  Games: 24  Percent:  2.4%  Cum: 67.3%
   [info] Score 19  Games: 27  Percent:  2.7%  Cum: 64.9%
   [info] Score 20  Games: 18  Percent:  1.8%  Cum: 62.2%
   [info] Score 21  Games: 29  Percent:  2.9%  Cum: 60.4%
   [info] Score 22  Games: 18  Percent:  1.8%  Cum: 57.5%
   [info] Score 23  Games: 37  Percent:  3.7%  Cum: 55.7%
   [info] Score 24  Games: 59  Percent:  5.9%  Cum: 52.0%
   [info] Score 25  Games: 461  Percent: 46.1%  Cum: 46.1%
   [info] Average Score: 19.576
   [info] Average Utility: 62.202
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  2  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.5%
   [info] Score  4  Games:  4  Percent:  0.4%  Cum: 99.5%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 99.1%
   [info] Score  6  Games:  8  Percent:  0.8%  Cum: 98.4%
   [info] Score  7  Games:  8  Percent:  0.8%  Cum: 97.6%
   [info] Score  8  Games: 16  Percent:  1.6%  Cum: 96.8%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 95.2%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 94.0%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 91.6%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 89.4%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 87.5%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 85.2%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 82.1%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 79.8%
   [info] Score 17  Games: 26  Percent:  2.6%  Cum: 76.9%
   [info] Score 18  Games: 32  Percent:  3.2%  Cum: 74.3%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 71.1%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 68.1%
   [info] Score 21  Games: 34  Percent:  3.4%  Cum: 64.6%
   [info] Score 22  Games: 56  Percent:  5.6%  Cum: 61.2%
   [info] Score 23  Games: 61  Percent:  6.1%  Cum: 55.6%
   [info] Score 24  Games: 159  Percent: 15.9%  Cum: 49.5%
   [info] Score 25  Games: 336  Percent: 33.6%  Cum: 33.6%
   [info] Average Score: 20.502
   [info] Average Utility: 57.804
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.7%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.6%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.4%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 99.1%
   [info] Score  6  Games:  8  Percent:  0.8%  Cum: 98.6%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 97.8%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 96.8%
   [info] Score  9  Games: 21  Percent:  2.1%  Cum: 95.0%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 92.9%
   [info] Score 11  Games: 32  Percent:  3.2%  Cum: 91.2%
   [info] Score 12  Games: 33  Percent:  3.3%  Cum: 88.0%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 84.7%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 82.6%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 80.1%
   [info] Score 16  Games: 39  Percent:  3.9%  Cum: 77.0%
   [info] Score 17  Games: 39  Percent:  3.9%  Cum: 73.1%
   [info] Score 18  Games: 40  Percent:  4.0%  Cum: 69.2%
   [info] Score 19  Games: 39  Percent:  3.9%  Cum: 65.2%
   [info] Score 20  Games: 34  Percent:  3.4%  Cum: 61.3%
   [info] Score 21  Games: 51  Percent:  5.1%  Cum: 57.9%
   [info] Score 22  Games: 91  Percent:  9.1%  Cum: 52.8%
   [info] Score 23  Games: 107  Percent: 10.7%  Cum: 43.7%
   [info] Score 24  Games: 192  Percent: 19.2%  Cum: 33.0%
   [info] Score 25  Games: 138  Percent: 13.8%  Cum: 13.8%
   [info] Average Score: 19.525
   [info] Average Utility: 45.95
   [info]
   [info] Time: 1446.516908178
      */

}
