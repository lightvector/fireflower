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
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 94.5%
   [info] Score  7  Games: 16  Percent:  1.6%  Cum: 93.1%
   [info] Score  8  Games: 22  Percent:  2.2%  Cum: 91.5%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 89.3%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 87.9%
   [info] Score 11  Games: 16  Percent:  1.6%  Cum: 86.1%
   [info] Score 12  Games: 20  Percent:  2.0%  Cum: 84.5%
   [info] Score 13  Games: 26  Percent:  2.6%  Cum: 82.5%
   [info] Score 14  Games: 23  Percent:  2.3%  Cum: 79.9%
   [info] Score 15  Games: 15  Percent:  1.5%  Cum: 77.6%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 76.1%
   [info] Score 17  Games: 17  Percent:  1.7%  Cum: 73.0%
   [info] Score 18  Games: 24  Percent:  2.4%  Cum: 71.3%
   [info] Score 19  Games: 22  Percent:  2.2%  Cum: 68.9%
   [info] Score 20  Games: 18  Percent:  1.8%  Cum: 66.7%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 64.9%
   [info] Score 22  Games: 32  Percent:  3.2%  Cum: 62.3%
   [info] Score 23  Games: 39  Percent:  3.9%  Cum: 59.1%
   [info] Score 24  Games: 73  Percent:  7.3%  Cum: 55.2%
   [info] Score 25  Games: 479  Percent: 47.9%  Cum: 47.9%
   [info] Average Score: 20.055
   [info] Average Utility: 64.06
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
   [info] Score  8  Games:  9  Percent:  0.9%  Cum: 96.9%
   [info] Score  9  Games: 22  Percent:  2.2%  Cum: 96.0%
   [info] Score 10  Games: 10  Percent:  1.0%  Cum: 93.8%
   [info] Score 11  Games: 28  Percent:  2.8%  Cum: 92.8%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 90.0%
   [info] Score 13  Games: 15  Percent:  1.5%  Cum: 87.6%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 86.1%
   [info] Score 15  Games: 27  Percent:  2.7%  Cum: 83.9%
   [info] Score 16  Games: 29  Percent:  2.9%  Cum: 81.2%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 78.3%
   [info] Score 18  Games: 22  Percent:  2.2%  Cum: 76.0%
   [info] Score 19  Games: 36  Percent:  3.6%  Cum: 73.8%
   [info] Score 20  Games: 33  Percent:  3.3%  Cum: 70.2%
   [info] Score 21  Games: 34  Percent:  3.4%  Cum: 66.9%
   [info] Score 22  Games: 56  Percent:  5.6%  Cum: 63.5%
   [info] Score 23  Games: 73  Percent:  7.3%  Cum: 57.9%
   [info] Score 24  Games: 179  Percent: 17.9%  Cum: 50.6%
   [info] Score 25  Games: 327  Percent: 32.7%  Cum: 32.7%
   [info] Average Score: 20.739
   [info] Average Utility: 57.828
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.6%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.6%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 99.0%
   [info] Score  6  Games:  7  Percent:  0.7%  Cum: 98.4%
   [info] Score  7  Games: 14  Percent:  1.4%  Cum: 97.7%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 96.3%
   [info] Score  9  Games: 17  Percent:  1.7%  Cum: 94.8%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 93.1%
   [info] Score 11  Games: 26  Percent:  2.6%  Cum: 91.1%
   [info] Score 12  Games: 18  Percent:  1.8%  Cum: 88.5%
   [info] Score 13  Games: 23  Percent:  2.3%  Cum: 86.7%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 84.4%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 82.2%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 79.9%
   [info] Score 17  Games: 32  Percent:  3.2%  Cum: 76.1%
   [info] Score 18  Games: 37  Percent:  3.7%  Cum: 72.9%
   [info] Score 19  Games: 45  Percent:  4.5%  Cum: 69.2%
   [info] Score 20  Games: 53  Percent:  5.3%  Cum: 64.7%
   [info] Score 21  Games: 56  Percent:  5.6%  Cum: 59.4%
   [info] Score 22  Games: 100  Percent: 10.0%  Cum: 53.8%
   [info] Score 23  Games: 107  Percent: 10.7%  Cum: 43.8%
   [info] Score 24  Games: 185  Percent: 18.5%  Cum: 33.1%
   [info] Score 25  Games: 146  Percent: 14.6%  Cum: 14.6%
   [info] Average Score: 19.786
   [info] Average Utility: 46.872
   [info]
   [info] Time: 1479.90735122

   */

}
