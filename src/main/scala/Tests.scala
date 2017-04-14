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
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  3  Games: 11  Percent:  1.1%  Cum: 99.3%
   [info] Score  4  Games: 15  Percent:  1.5%  Cum: 98.2%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 96.7%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 94.7%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 93.1%
   [info] Score  8  Games: 27  Percent:  2.7%  Cum: 90.7%
   [info] Score  9  Games: 30  Percent:  3.0%  Cum: 88.0%
   [info] Score 10  Games: 43  Percent:  4.3%  Cum: 85.0%
   [info] Score 11  Games: 33  Percent:  3.3%  Cum: 80.7%
   [info] Score 12  Games: 23  Percent:  2.3%  Cum: 77.4%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 75.1%
   [info] Score 14  Games: 23  Percent:  2.3%  Cum: 72.7%
   [info] Score 15  Games: 30  Percent:  3.0%  Cum: 70.4%
   [info] Score 16  Games: 23  Percent:  2.3%  Cum: 67.4%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 65.1%
   [info] Score 18  Games: 25  Percent:  2.5%  Cum: 62.8%
   [info] Score 19  Games: 23  Percent:  2.3%  Cum: 60.3%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 58.0%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 55.8%
   [info] Score 22  Games: 25  Percent:  2.5%  Cum: 53.2%
   [info] Score 23  Games: 31  Percent:  3.1%  Cum: 50.7%
   [info] Score 24  Games: 92  Percent:  9.2%  Cum: 47.6%
   [info] Score 25  Games: 384  Percent: 38.4%  Cum: 38.4%
   [info] Average Score: 18.811
   [info] Average Utility: 56.822
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.6%
   [info] Score  7  Games:  9  Percent:  0.9%  Cum: 98.2%
   [info] Score  8  Games:  7  Percent:  0.7%  Cum: 97.3%
   [info] Score  9  Games: 13  Percent:  1.3%  Cum: 96.6%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 95.3%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 93.5%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 91.4%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 88.5%
   [info] Score 14  Games: 19  Percent:  1.9%  Cum: 86.4%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 84.5%
   [info] Score 16  Games: 35  Percent:  3.5%  Cum: 82.4%
   [info] Score 17  Games: 34  Percent:  3.4%  Cum: 78.9%
   [info] Score 18  Games: 31  Percent:  3.1%  Cum: 75.5%
   [info] Score 19  Games: 35  Percent:  3.5%  Cum: 72.4%
   [info] Score 20  Games: 37  Percent:  3.7%  Cum: 68.9%
   [info] Score 21  Games: 64  Percent:  6.4%  Cum: 65.2%
   [info] Score 22  Games: 72  Percent:  7.2%  Cum: 58.8%
   [info] Score 23  Games: 72  Percent:  7.2%  Cum: 51.6%
   [info] Score 24  Games: 204  Percent: 20.4%  Cum: 44.4%
   [info] Score 25  Games: 240  Percent: 24.0%  Cum: 24.0%
   [info] Average Score: 20.508
   [info] Average Utility: 53.016
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  3  Games:  6  Percent:  0.6%  Cum: 99.8%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.2%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 98.6%
   [info] Score  6  Games:  8  Percent:  0.8%  Cum: 97.9%
   [info] Score  7  Games:  6  Percent:  0.6%  Cum: 97.1%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 96.5%
   [info] Score  9  Games: 20  Percent:  2.0%  Cum: 94.8%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 92.8%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 90.6%
   [info] Score 12  Games: 23  Percent:  2.3%  Cum: 87.8%
   [info] Score 13  Games: 33  Percent:  3.3%  Cum: 85.5%
   [info] Score 14  Games: 27  Percent:  2.7%  Cum: 82.2%
   [info] Score 15  Games: 33  Percent:  3.3%  Cum: 79.5%
   [info] Score 16  Games: 42  Percent:  4.2%  Cum: 76.2%
   [info] Score 17  Games: 43  Percent:  4.3%  Cum: 72.0%
   [info] Score 18  Games: 58  Percent:  5.8%  Cum: 67.7%
   [info] Score 19  Games: 44  Percent:  4.4%  Cum: 61.9%
   [info] Score 20  Games: 63  Percent:  6.3%  Cum: 57.5%
   [info] Score 21  Games: 63  Percent:  6.3%  Cum: 51.2%
   [info] Score 22  Games: 90  Percent:  9.0%  Cum: 44.9%
   [info] Score 23  Games: 128  Percent: 12.8%  Cum: 35.9%
   [info] Score 24  Games: 159  Percent: 15.9%  Cum: 23.1%
   [info] Score 25  Games: 72  Percent:  7.2%  Cum:  7.2%
   [info] Average Score: 18.997
   [info] Average Utility: 41.594
   [info]
   [info] Time: 1206.61335214

   */

}
