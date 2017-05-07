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
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.7%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 99.1%
   [info] Score  4  Games: 10  Percent:  1.0%  Cum: 98.1%
   [info] Score  5  Games: 24  Percent:  2.4%  Cum: 97.1%
   [info] Score  6  Games: 15  Percent:  1.5%  Cum: 94.7%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 93.2%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 91.2%
   [info] Score  9  Games: 14  Percent:  1.4%  Cum: 89.4%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 88.0%
   [info] Score 11  Games: 12  Percent:  1.2%  Cum: 86.0%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 84.8%
   [info] Score 13  Games: 16  Percent:  1.6%  Cum: 82.9%
   [info] Score 14  Games: 21  Percent:  2.1%  Cum: 81.3%
   [info] Score 15  Games: 10  Percent:  1.0%  Cum: 79.2%
   [info] Score 16  Games: 24  Percent:  2.4%  Cum: 78.2%
   [info] Score 17  Games: 17  Percent:  1.7%  Cum: 75.8%
   [info] Score 18  Games: 21  Percent:  2.1%  Cum: 74.1%
   [info] Score 19  Games: 20  Percent:  2.0%  Cum: 72.0%
   [info] Score 20  Games: 14  Percent:  1.4%  Cum: 70.0%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 68.6%
   [info] Score 22  Games: 35  Percent:  3.5%  Cum: 66.0%
   [info] Score 23  Games: 44  Percent:  4.4%  Cum: 62.5%
   [info] Score 24  Games: 82  Percent:  8.2%  Cum: 58.1%
   [info] Score 25  Games: 499  Percent: 49.9%  Cum: 49.9%
   [info] Average Score: 20.399
   [info] Average Utility: 65.748
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 99.9%
   [info] Score  2  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  3  Percent:  0.3%  Cum: 99.6%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.3%
   [info] Score  6  Games: 13  Percent:  1.3%  Cum: 98.9%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 97.6%
   [info] Score  8  Games: 10  Percent:  1.0%  Cum: 96.6%
   [info] Score  9  Games: 11  Percent:  1.1%  Cum: 95.6%
   [info] Score 10  Games: 10  Percent:  1.0%  Cum: 94.5%
   [info] Score 11  Games: 12  Percent:  1.2%  Cum: 93.5%
   [info] Score 12  Games: 19  Percent:  1.9%  Cum: 92.3%
   [info] Score 13  Games: 11  Percent:  1.1%  Cum: 90.4%
   [info] Score 14  Games: 18  Percent:  1.8%  Cum: 89.3%
   [info] Score 15  Games: 11  Percent:  1.1%  Cum: 87.5%
   [info] Score 16  Games: 23  Percent:  2.3%  Cum: 86.4%
   [info] Score 17  Games: 24  Percent:  2.4%  Cum: 84.1%
   [info] Score 18  Games: 23  Percent:  2.3%  Cum: 81.7%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 79.4%
   [info] Score 20  Games: 37  Percent:  3.7%  Cum: 76.4%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 72.7%
   [info] Score 22  Games: 73  Percent:  7.3%  Cum: 70.0%
   [info] Score 23  Games: 90  Percent:  9.0%  Cum: 62.7%
   [info] Score 24  Games: 176  Percent: 17.6%  Cum: 53.7%
   [info] Score 25  Games: 361  Percent: 36.1%  Cum: 36.1%
   [info] Average Score: 21.378
   [info] Average Utility: 60.806
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  0  Percent:  0.0%  Cum: 99.8%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  5  Games:  4  Percent:  0.4%  Cum: 99.3%
   [info] Score  6  Games:  6  Percent:  0.6%  Cum: 98.9%
   [info] Score  7  Games: 10  Percent:  1.0%  Cum: 98.3%
   [info] Score  8  Games: 10  Percent:  1.0%  Cum: 97.3%
   [info] Score  9  Games: 17  Percent:  1.7%  Cum: 96.3%
   [info] Score 10  Games: 13  Percent:  1.3%  Cum: 94.6%
   [info] Score 11  Games: 20  Percent:  2.0%  Cum: 93.3%
   [info] Score 12  Games: 14  Percent:  1.4%  Cum: 91.3%
   [info] Score 13  Games: 20  Percent:  2.0%  Cum: 89.9%
   [info] Score 14  Games: 19  Percent:  1.9%  Cum: 87.9%
   [info] Score 15  Games: 29  Percent:  2.9%  Cum: 86.0%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 83.1%
   [info] Score 17  Games: 28  Percent:  2.8%  Cum: 80.0%
   [info] Score 18  Games: 45  Percent:  4.5%  Cum: 77.2%
   [info] Score 19  Games: 40  Percent:  4.0%  Cum: 72.7%
   [info] Score 20  Games: 53  Percent:  5.3%  Cum: 68.7%
   [info] Score 21  Games: 56  Percent:  5.6%  Cum: 63.4%
   [info] Score 22  Games: 103  Percent: 10.3%  Cum: 57.8%
   [info] Score 23  Games: 122  Percent: 12.2%  Cum: 47.5%
   [info] Score 24  Games: 200  Percent: 20.0%  Cum: 35.3%
   [info] Score 25  Games: 153  Percent: 15.3%  Cum: 15.3%
   [info] Average Score: 20.337
   [info] Average Utility: 48.324
   [info]
   [info] Time: 1550.645487626

   */

}
