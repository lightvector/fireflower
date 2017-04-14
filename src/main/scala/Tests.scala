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
   [info] Score  2  Games: 10  Percent:  1.0%  Cum: 99.2%
   [info] Score  3  Games: 22  Percent:  2.2%  Cum: 98.2%
   [info] Score  4  Games: 18  Percent:  1.8%  Cum: 96.0%
   [info] Score  5  Games: 27  Percent:  2.7%  Cum: 94.2%
   [info] Score  6  Games: 23  Percent:  2.3%  Cum: 91.5%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 89.2%
   [info] Score  8  Games: 29  Percent:  2.9%  Cum: 86.9%
   [info] Score  9  Games: 19  Percent:  1.9%  Cum: 84.0%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 82.1%
   [info] Score 11  Games: 30  Percent:  3.0%  Cum: 80.4%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 77.4%
   [info] Score 13  Games: 27  Percent:  2.7%  Cum: 75.0%
   [info] Score 14  Games: 24  Percent:  2.4%  Cum: 72.3%
   [info] Score 15  Games: 26  Percent:  2.6%  Cum: 69.9%
   [info] Score 16  Games: 21  Percent:  2.1%  Cum: 67.3%
   [info] Score 17  Games: 31  Percent:  3.1%  Cum: 65.2%
   [info] Score 18  Games: 33  Percent:  3.3%  Cum: 62.1%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 58.8%
   [info] Score 20  Games: 23  Percent:  2.3%  Cum: 56.7%
   [info] Score 21  Games: 29  Percent:  2.9%  Cum: 54.4%
   [info] Score 22  Games: 24  Percent:  2.4%  Cum: 51.5%
   [info] Score 23  Games: 27  Percent:  2.7%  Cum: 49.1%
   [info] Score 24  Games: 92  Percent:  9.2%  Cum: 46.4%
   [info] Score 25  Games: 372  Percent: 37.2%  Cum: 37.2%
   [info] Average Score: 18.447
   [info] Average Utility: 55.494
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.7%
   [info] Score  2  Games:  3  Percent:  0.3%  Cum: 99.2%
   [info] Score  3  Games:  9  Percent:  0.9%  Cum: 98.9%
   [info] Score  4  Games: 11  Percent:  1.1%  Cum: 98.0%
   [info] Score  5  Games: 10  Percent:  1.0%  Cum: 96.9%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 95.9%
   [info] Score  7  Games: 12  Percent:  1.2%  Cum: 94.5%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 93.3%
   [info] Score  9  Games: 11  Percent:  1.1%  Cum: 91.6%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 90.5%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 88.8%
   [info] Score 12  Games: 18  Percent:  1.8%  Cum: 86.6%
   [info] Score 13  Games: 21  Percent:  2.1%  Cum: 84.8%
   [info] Score 14  Games: 18  Percent:  1.8%  Cum: 82.7%
   [info] Score 15  Games: 19  Percent:  1.9%  Cum: 80.9%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 79.0%
   [info] Score 17  Games: 20  Percent:  2.0%  Cum: 76.1%
   [info] Score 18  Games: 39  Percent:  3.9%  Cum: 74.1%
   [info] Score 19  Games: 36  Percent:  3.6%  Cum: 70.2%
   [info] Score 20  Games: 33  Percent:  3.3%  Cum: 66.6%
   [info] Score 21  Games: 44  Percent:  4.4%  Cum: 63.3%
   [info] Score 22  Games: 83  Percent:  8.3%  Cum: 58.9%
   [info] Score 23  Games: 61  Percent:  6.1%  Cum: 50.6%
   [info] Score 24  Games: 219  Percent: 21.9%  Cum: 44.5%
   [info] Score 25  Games: 226  Percent: 22.6%  Cum: 22.6%
   [info] Average Score: 19.882
   [info] Average Utility: 51.064
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.3%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.6%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 97.6%
   [info] Score  5  Games: 12  Percent:  1.2%  Cum: 96.9%
   [info] Score  6  Games: 10  Percent:  1.0%  Cum: 95.7%
   [info] Score  7  Games: 30  Percent:  3.0%  Cum: 94.7%
   [info] Score  8  Games: 28  Percent:  2.8%  Cum: 91.7%
   [info] Score  9  Games: 17  Percent:  1.7%  Cum: 88.9%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 87.2%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 85.5%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 83.4%
   [info] Score 13  Games: 25  Percent:  2.5%  Cum: 80.5%
   [info] Score 14  Games: 30  Percent:  3.0%  Cum: 78.0%
   [info] Score 15  Games: 34  Percent:  3.4%  Cum: 75.0%
   [info] Score 16  Games: 34  Percent:  3.4%  Cum: 71.6%
   [info] Score 17  Games: 45  Percent:  4.5%  Cum: 68.2%
   [info] Score 18  Games: 52  Percent:  5.2%  Cum: 63.7%
   [info] Score 19  Games: 59  Percent:  5.9%  Cum: 58.5%
   [info] Score 20  Games: 66  Percent:  6.6%  Cum: 52.6%
   [info] Score 21  Games: 58  Percent:  5.8%  Cum: 46.0%
   [info] Score 22  Games: 85  Percent:  8.5%  Cum: 40.2%
   [info] Score 23  Games: 103  Percent: 10.3%  Cum: 31.7%
   [info] Score 24  Games: 151  Percent: 15.1%  Cum: 21.4%
   [info] Score 25  Games: 63  Percent:  6.3%  Cum:  6.3%
   [info] Average Score: 18.13
   [info] Average Utility: 39.41
   [info]
   [info] Time: 1007.818289981
      */

}
