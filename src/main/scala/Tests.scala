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

    runTests(prefix="",salt="d",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 100.0%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.6%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.9%
   [info] Score  4  Games: 11  Percent:  1.1%  Cum: 97.9%
   [info] Score  5  Games: 23  Percent:  2.3%  Cum: 96.8%
   [info] Score  6  Games: 13  Percent:  1.3%  Cum: 94.5%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 93.2%
   [info] Score  8  Games: 25  Percent:  2.5%  Cum: 91.7%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 89.2%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 87.6%
   [info] Score 11  Games: 23  Percent:  2.3%  Cum: 85.4%
   [info] Score 12  Games: 30  Percent:  3.0%  Cum: 83.1%
   [info] Score 13  Games: 25  Percent:  2.5%  Cum: 80.1%
   [info] Score 14  Games: 29  Percent:  2.9%  Cum: 77.6%
   [info] Score 15  Games: 18  Percent:  1.8%  Cum: 74.7%
   [info] Score 16  Games: 28  Percent:  2.8%  Cum: 72.9%
   [info] Score 17  Games: 20  Percent:  2.0%  Cum: 70.1%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 68.1%
   [info] Score 19  Games: 28  Percent:  2.8%  Cum: 65.5%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 62.7%
   [info] Score 21  Games: 23  Percent:  2.3%  Cum: 60.8%
   [info] Score 22  Games: 32  Percent:  3.2%  Cum: 58.5%
   [info] Score 23  Games: 38  Percent:  3.8%  Cum: 55.3%
   [info] Score 24  Games: 71  Percent:  7.1%  Cum: 51.5%
   [info] Score 25  Games: 444  Percent: 44.4%  Cum: 44.4%
   [info] Average Score: 19.601
   [info] Average Utility: 61.402
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.9%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.9%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 99.4%
   [info] Score  6  Games:  8  Percent:  0.8%  Cum: 98.8%
   [info] Score  7  Games: 11  Percent:  1.1%  Cum: 98.0%
   [info] Score  8  Games: 10  Percent:  1.0%  Cum: 96.9%
   [info] Score  9  Games: 22  Percent:  2.2%  Cum: 95.9%
   [info] Score 10  Games: 10  Percent:  1.0%  Cum: 93.7%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 92.7%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 90.0%
   [info] Score 13  Games: 17  Percent:  1.7%  Cum: 87.6%
   [info] Score 14  Games: 21  Percent:  2.1%  Cum: 85.9%
   [info] Score 15  Games: 28  Percent:  2.8%  Cum: 83.8%
   [info] Score 16  Games: 27  Percent:  2.7%  Cum: 81.0%
   [info] Score 17  Games: 26  Percent:  2.6%  Cum: 78.3%
   [info] Score 18  Games: 23  Percent:  2.3%  Cum: 75.7%
   [info] Score 19  Games: 31  Percent:  3.1%  Cum: 73.4%
   [info] Score 20  Games: 29  Percent:  2.9%  Cum: 70.3%
   [info] Score 21  Games: 33  Percent:  3.3%  Cum: 67.4%
   [info] Score 22  Games: 59  Percent:  5.9%  Cum: 64.1%
   [info] Score 23  Games: 74  Percent:  7.4%  Cum: 58.2%
   [info] Score 24  Games: 179  Percent: 17.9%  Cum: 50.8%
   [info] Score 25  Games: 329  Percent: 32.9%  Cum: 32.9%
   [info] Average Score: 20.743
   [info] Average Utility: 57.936
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.6%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.6%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 98.9%
   [info] Score  6  Games:  6  Percent:  0.6%  Cum: 98.2%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 97.6%
   [info] Score  8  Games: 17  Percent:  1.7%  Cum: 96.1%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 94.4%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 92.0%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 90.2%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 88.1%
   [info] Score 13  Games: 31  Percent:  3.1%  Cum: 85.2%
   [info] Score 14  Games: 35  Percent:  3.5%  Cum: 82.1%
   [info] Score 15  Games: 35  Percent:  3.5%  Cum: 78.6%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 75.1%
   [info] Score 17  Games: 31  Percent:  3.1%  Cum: 71.3%
   [info] Score 18  Games: 30  Percent:  3.0%  Cum: 68.2%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 65.2%
   [info] Score 20  Games: 51  Percent:  5.1%  Cum: 60.2%
   [info] Score 21  Games: 45  Percent:  4.5%  Cum: 55.1%
   [info] Score 22  Games: 98  Percent:  9.8%  Cum: 50.6%
   [info] Score 23  Games: 95  Percent:  9.5%  Cum: 40.8%
   [info] Score 24  Games: 186  Percent: 18.6%  Cum: 31.3%
   [info] Score 25  Games: 127  Percent: 12.7%  Cum: 12.7%
   [info] Average Score: 19.308
   [info] Average Utility: 44.966
   [info]
   [info] Time: 1411.360518978
   */

}
