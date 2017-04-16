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
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 100.0%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 99.4%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 98.2%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 97.5%
   [info] Score  6  Games: 15  Percent:  1.5%  Cum: 95.5%
   [info] Score  7  Games: 24  Percent:  2.4%  Cum: 94.0%
   [info] Score  8  Games: 33  Percent:  3.3%  Cum: 91.6%
   [info] Score  9  Games: 30  Percent:  3.0%  Cum: 88.3%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 85.3%
   [info] Score 11  Games: 17  Percent:  1.7%  Cum: 83.1%
   [info] Score 12  Games: 12  Percent:  1.2%  Cum: 81.4%
   [info] Score 13  Games: 27  Percent:  2.7%  Cum: 80.2%
   [info] Score 14  Games: 21  Percent:  2.1%  Cum: 77.5%
   [info] Score 15  Games: 27  Percent:  2.7%  Cum: 75.4%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 72.7%
   [info] Score 17  Games: 21  Percent:  2.1%  Cum: 70.0%
   [info] Score 18  Games: 19  Percent:  1.9%  Cum: 67.9%
   [info] Score 19  Games: 29  Percent:  2.9%  Cum: 66.0%
   [info] Score 20  Games: 28  Percent:  2.8%  Cum: 63.1%
   [info] Score 21  Games: 24  Percent:  2.4%  Cum: 60.3%
   [info] Score 22  Games: 25  Percent:  2.5%  Cum: 57.9%
   [info] Score 23  Games: 33  Percent:  3.3%  Cum: 55.4%
   [info] Score 24  Games: 85  Percent:  8.5%  Cum: 52.1%
   [info] Score 25  Games: 436  Percent: 43.6%  Cum: 43.6%
   [info] Average Score: 19.564
   [info] Average Utility: 60.928
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  2  Games:  3  Percent:  0.3%  Cum: 99.9%
   [info] Score  3  Games:  4  Percent:  0.4%  Cum: 99.6%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.2%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 98.9%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.5%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 98.0%
   [info] Score  8  Games: 12  Percent:  1.2%  Cum: 97.0%
   [info] Score  9  Games: 10  Percent:  1.0%  Cum: 95.8%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 94.8%
   [info] Score 11  Games: 11  Percent:  1.1%  Cum: 92.6%
   [info] Score 12  Games: 23  Percent:  2.3%  Cum: 91.5%
   [info] Score 13  Games: 19  Percent:  1.9%  Cum: 89.2%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 87.3%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 85.1%
   [info] Score 16  Games: 36  Percent:  3.6%  Cum: 83.0%
   [info] Score 17  Games: 15  Percent:  1.5%  Cum: 79.4%
   [info] Score 18  Games: 32  Percent:  3.2%  Cum: 77.9%
   [info] Score 19  Games: 44  Percent:  4.4%  Cum: 74.7%
   [info] Score 20  Games: 43  Percent:  4.3%  Cum: 70.3%
   [info] Score 21  Games: 41  Percent:  4.1%  Cum: 66.0%
   [info] Score 22  Games: 82  Percent:  8.2%  Cum: 61.9%
   [info] Score 23  Games: 77  Percent:  7.7%  Cum: 53.7%
   [info] Score 24  Games: 197  Percent: 19.7%  Cum: 46.0%
   [info] Score 25  Games: 263  Percent: 26.3%  Cum: 26.3%
   [info] Average Score: 20.665
   [info] Average Utility: 54.48
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.6%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 99.0%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.5%
   [info] Score  7  Games:  9  Percent:  0.9%  Cum: 98.0%
   [info] Score  8  Games: 12  Percent:  1.2%  Cum: 97.1%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 95.9%
   [info] Score 10  Games: 15  Percent:  1.5%  Cum: 94.7%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 93.2%
   [info] Score 12  Games: 26  Percent:  2.6%  Cum: 91.1%
   [info] Score 13  Games: 40  Percent:  4.0%  Cum: 88.5%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 84.5%
   [info] Score 15  Games: 34  Percent:  3.4%  Cum: 81.1%
   [info] Score 16  Games: 33  Percent:  3.3%  Cum: 77.7%
   [info] Score 17  Games: 51  Percent:  5.1%  Cum: 74.4%
   [info] Score 18  Games: 44  Percent:  4.4%  Cum: 69.3%
   [info] Score 19  Games: 45  Percent:  4.5%  Cum: 64.9%
   [info] Score 20  Games: 60  Percent:  6.0%  Cum: 60.4%
   [info] Score 21  Games: 52  Percent:  5.2%  Cum: 54.4%
   [info] Score 22  Games: 87  Percent:  8.7%  Cum: 49.2%
   [info] Score 23  Games: 122  Percent: 12.2%  Cum: 40.5%
   [info] Score 24  Games: 204  Percent: 20.4%  Cum: 28.3%
   [info] Score 25  Games: 79  Percent:  7.9%  Cum:  7.9%
   [info] Average Score: 19.476
   [info] Average Utility: 42.902
   [info]
   [info] Time: 1245.087468005

   */

}
