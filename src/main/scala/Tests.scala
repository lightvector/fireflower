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
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.2%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 97.4%
   [info] Score  6  Games: 19  Percent:  1.9%  Cum: 95.2%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 93.3%
   [info] Score  8  Games: 32  Percent:  3.2%  Cum: 91.1%
   [info] Score  9  Games: 28  Percent:  2.8%  Cum: 87.9%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 85.1%
   [info] Score 11  Games: 17  Percent:  1.7%  Cum: 82.9%
   [info] Score 12  Games: 14  Percent:  1.4%  Cum: 81.2%
   [info] Score 13  Games: 32  Percent:  3.2%  Cum: 79.8%
   [info] Score 14  Games: 20  Percent:  2.0%  Cum: 76.6%
   [info] Score 15  Games: 26  Percent:  2.6%  Cum: 74.6%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 72.0%
   [info] Score 17  Games: 20  Percent:  2.0%  Cum: 69.1%
   [info] Score 18  Games: 23  Percent:  2.3%  Cum: 67.1%
   [info] Score 19  Games: 27  Percent:  2.7%  Cum: 64.8%
   [info] Score 20  Games: 26  Percent:  2.6%  Cum: 62.1%
   [info] Score 21  Games: 23  Percent:  2.3%  Cum: 59.5%
   [info] Score 22  Games: 24  Percent:  2.4%  Cum: 57.2%
   [info] Score 23  Games: 40  Percent:  4.0%  Cum: 54.8%
   [info] Score 24  Games: 85  Percent:  8.5%  Cum: 50.8%
   [info] Score 25  Games: 423  Percent: 42.3%  Cum: 42.3%
   [info] Average Score: 19.424
   [info] Average Utility: 59.998
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  2  Games:  3  Percent:  0.3%  Cum: 99.9%
   [info] Score  3  Games:  4  Percent:  0.4%  Cum: 99.6%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.2%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 98.9%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.5%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 97.8%
   [info] Score  8  Games: 12  Percent:  1.2%  Cum: 96.8%
   [info] Score  9  Games: 10  Percent:  1.0%  Cum: 95.6%
   [info] Score 10  Games: 23  Percent:  2.3%  Cum: 94.6%
   [info] Score 11  Games: 11  Percent:  1.1%  Cum: 92.3%
   [info] Score 12  Games: 22  Percent:  2.2%  Cum: 91.2%
   [info] Score 13  Games: 22  Percent:  2.2%  Cum: 89.0%
   [info] Score 14  Games: 24  Percent:  2.4%  Cum: 86.8%
   [info] Score 15  Games: 20  Percent:  2.0%  Cum: 84.4%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 82.4%
   [info] Score 17  Games: 19  Percent:  1.9%  Cum: 78.6%
   [info] Score 18  Games: 37  Percent:  3.7%  Cum: 76.7%
   [info] Score 19  Games: 44  Percent:  4.4%  Cum: 73.0%
   [info] Score 20  Games: 39  Percent:  3.9%  Cum: 68.6%
   [info] Score 21  Games: 35  Percent:  3.5%  Cum: 64.7%
   [info] Score 22  Games: 77  Percent:  7.7%  Cum: 61.2%
   [info] Score 23  Games: 76  Percent:  7.6%  Cum: 53.5%
   [info] Score 24  Games: 196  Percent: 19.6%  Cum: 45.9%
   [info] Score 25  Games: 263  Percent: 26.3%  Cum: 26.3%
   [info] Average Score: 20.554
   [info] Average Utility: 54.258
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.8%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.7%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.7%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.6%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 98.1%
   [info] Score  8  Games: 11  Percent:  1.1%  Cum: 97.1%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 96.0%
   [info] Score 10  Games: 16  Percent:  1.6%  Cum: 94.6%
   [info] Score 11  Games: 19  Percent:  1.9%  Cum: 93.0%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 91.1%
   [info] Score 13  Games: 42  Percent:  4.2%  Cum: 88.6%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 84.4%
   [info] Score 15  Games: 37  Percent:  3.7%  Cum: 81.3%
   [info] Score 16  Games: 33  Percent:  3.3%  Cum: 77.6%
   [info] Score 17  Games: 55  Percent:  5.5%  Cum: 74.3%
   [info] Score 18  Games: 44  Percent:  4.4%  Cum: 68.8%
   [info] Score 19  Games: 48  Percent:  4.8%  Cum: 64.4%
   [info] Score 20  Games: 59  Percent:  5.9%  Cum: 59.6%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 53.7%
   [info] Score 22  Games: 85  Percent:  8.5%  Cum: 48.9%
   [info] Score 23  Games: 111  Percent: 11.1%  Cum: 40.4%
   [info] Score 24  Games: 201  Percent: 20.1%  Cum: 29.3%
   [info] Score 25  Games: 92  Percent:  9.2%  Cum:  9.2%
   [info] Average Score: 19.472
   [info] Average Utility: 43.544
   [info]
   [info] Time: 1115.822077268

   */

}
