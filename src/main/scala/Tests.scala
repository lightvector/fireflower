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
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 97.5%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 95.6%
   [info] Score  7  Games: 19  Percent:  1.9%  Cum: 94.0%
   [info] Score  8  Games: 31  Percent:  3.1%  Cum: 92.1%
   [info] Score  9  Games: 23  Percent:  2.3%  Cum: 89.0%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 86.7%
   [info] Score 11  Games: 19  Percent:  1.9%  Cum: 84.8%
   [info] Score 12  Games: 18  Percent:  1.8%  Cum: 82.9%
   [info] Score 13  Games: 31  Percent:  3.1%  Cum: 81.1%
   [info] Score 14  Games: 29  Percent:  2.9%  Cum: 78.0%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 75.1%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 72.7%
   [info] Score 17  Games: 17  Percent:  1.7%  Cum: 69.8%
   [info] Score 18  Games: 29  Percent:  2.9%  Cum: 68.1%
   [info] Score 19  Games: 32  Percent:  3.2%  Cum: 65.2%
   [info] Score 20  Games: 26  Percent:  2.6%  Cum: 62.0%
   [info] Score 21  Games: 31  Percent:  3.1%  Cum: 59.4%
   [info] Score 22  Games: 14  Percent:  1.4%  Cum: 56.3%
   [info] Score 23  Games: 30  Percent:  3.0%  Cum: 54.9%
   [info] Score 24  Games: 85  Percent:  8.5%  Cum: 51.9%
   [info] Score 25  Games: 434  Percent: 43.4%  Cum: 43.4%
   [info] Average Score: 19.584
   [info] Average Utility: 60.868
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score  4  Games:  1  Percent:  0.1%  Cum: 99.5%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 99.4%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.7%
   [info] Score  7  Games:  5  Percent:  0.5%  Cum: 98.3%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 97.8%
   [info] Score  9  Games: 15  Percent:  1.5%  Cum: 96.1%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 94.6%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 92.7%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 90.6%
   [info] Score 13  Games: 20  Percent:  2.0%  Cum: 88.7%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 86.7%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 84.2%
   [info] Score 16  Games: 33  Percent:  3.3%  Cum: 82.0%
   [info] Score 17  Games: 30  Percent:  3.0%  Cum: 78.7%
   [info] Score 18  Games: 39  Percent:  3.9%  Cum: 75.7%
   [info] Score 19  Games: 28  Percent:  2.8%  Cum: 71.8%
   [info] Score 20  Games: 32  Percent:  3.2%  Cum: 69.0%
   [info] Score 21  Games: 47  Percent:  4.7%  Cum: 65.8%
   [info] Score 22  Games: 68  Percent:  6.8%  Cum: 61.1%
   [info] Score 23  Games: 66  Percent:  6.6%  Cum: 54.3%
   [info] Score 24  Games: 176  Percent: 17.6%  Cum: 47.7%
   [info] Score 25  Games: 301  Percent: 30.1%  Cum: 30.1%
   [info] Average Score: 20.63
   [info] Average Utility: 56.31
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  3  Games:  1  Percent:  0.1%  Cum: 99.6%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.5%
   [info] Score  5  Games:  5  Percent:  0.5%  Cum: 99.0%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.5%
   [info] Score  7  Games: 14  Percent:  1.4%  Cum: 97.8%
   [info] Score  8  Games: 14  Percent:  1.4%  Cum: 96.4%
   [info] Score  9  Games: 22  Percent:  2.2%  Cum: 95.0%
   [info] Score 10  Games: 28  Percent:  2.8%  Cum: 92.8%
   [info] Score 11  Games: 40  Percent:  4.0%  Cum: 90.0%
   [info] Score 12  Games: 30  Percent:  3.0%  Cum: 86.0%
   [info] Score 13  Games: 26  Percent:  2.6%  Cum: 83.0%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 80.4%
   [info] Score 15  Games: 39  Percent:  3.9%  Cum: 77.3%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 73.4%
   [info] Score 17  Games: 39  Percent:  3.9%  Cum: 70.9%
   [info] Score 18  Games: 48  Percent:  4.8%  Cum: 67.0%
   [info] Score 19  Games: 44  Percent:  4.4%  Cum: 62.2%
   [info] Score 20  Games: 52  Percent:  5.2%  Cum: 57.8%
   [info] Score 21  Games: 45  Percent:  4.5%  Cum: 52.6%
   [info] Score 22  Games: 86  Percent:  8.6%  Cum: 48.1%
   [info] Score 23  Games: 109  Percent: 10.9%  Cum: 39.5%
   [info] Score 24  Games: 181  Percent: 18.1%  Cum: 28.6%
   [info] Score 25  Games: 105  Percent: 10.5%  Cum: 10.5%
   [info] Average Score: 19.057
   [info] Average Utility: 43.364
   [info]
   [info] Time: 1162.472064714

   */

}
