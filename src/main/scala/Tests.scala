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
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  3  Percent:  0.3%  Cum: 99.8%
   [info] Score  2  Games: 13  Percent:  1.3%  Cum: 99.5%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.2%
   [info] Score  4  Games: 18  Percent:  1.8%  Cum: 96.6%
   [info] Score  5  Games: 25  Percent:  2.5%  Cum: 94.8%
   [info] Score  6  Games: 24  Percent:  2.4%  Cum: 92.3%
   [info] Score  7  Games: 30  Percent:  3.0%  Cum: 89.9%
   [info] Score  8  Games: 30  Percent:  3.0%  Cum: 86.9%
   [info] Score  9  Games: 28  Percent:  2.8%  Cum: 83.9%
   [info] Score 10  Games: 15  Percent:  1.5%  Cum: 81.1%
   [info] Score 11  Games: 30  Percent:  3.0%  Cum: 79.6%
   [info] Score 12  Games: 29  Percent:  2.9%  Cum: 76.6%
   [info] Score 13  Games: 16  Percent:  1.6%  Cum: 73.7%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 72.1%
   [info] Score 15  Games: 27  Percent:  2.7%  Cum: 69.0%
   [info] Score 16  Games: 26  Percent:  2.6%  Cum: 66.3%
   [info] Score 17  Games: 26  Percent:  2.6%  Cum: 63.7%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 61.1%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 58.4%
   [info] Score 20  Games: 29  Percent:  2.9%  Cum: 56.3%
   [info] Score 21  Games: 30  Percent:  3.0%  Cum: 53.4%
   [info] Score 22  Games: 24  Percent:  2.4%  Cum: 50.4%
   [info] Score 23  Games: 38  Percent:  3.8%  Cum: 48.0%
   [info] Score 24  Games: 76  Percent:  7.6%  Cum: 44.2%
   [info] Score 25  Games: 366  Percent: 36.6%  Cum: 36.6%
   [info] Average Score: 18.324
   [info] Average Utility: 54.948
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.9%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.0%
   [info] Score  3  Games: 11  Percent:  1.1%  Cum: 98.6%
   [info] Score  4  Games: 14  Percent:  1.4%  Cum: 97.5%
   [info] Score  5  Games: 12  Percent:  1.2%  Cum: 96.1%
   [info] Score  6  Games: 20  Percent:  2.0%  Cum: 94.9%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 92.9%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 91.4%
   [info] Score  9  Games: 18  Percent:  1.8%  Cum: 89.9%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 88.1%
   [info] Score 11  Games: 27  Percent:  2.7%  Cum: 85.9%
   [info] Score 12  Games: 17  Percent:  1.7%  Cum: 83.2%
   [info] Score 13  Games: 22  Percent:  2.2%  Cum: 81.5%
   [info] Score 14  Games: 25  Percent:  2.5%  Cum: 79.3%
   [info] Score 15  Games: 35  Percent:  3.5%  Cum: 76.8%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 73.3%
   [info] Score 17  Games: 34  Percent:  3.4%  Cum: 70.2%
   [info] Score 18  Games: 31  Percent:  3.1%  Cum: 66.8%
   [info] Score 19  Games: 37  Percent:  3.7%  Cum: 63.7%
   [info] Score 20  Games: 30  Percent:  3.0%  Cum: 60.0%
   [info] Score 21  Games: 31  Percent:  3.1%  Cum: 57.0%
   [info] Score 22  Games: 68  Percent:  6.8%  Cum: 53.9%
   [info] Score 23  Games: 71  Percent:  7.1%  Cum: 47.1%
   [info] Score 24  Games: 200  Percent: 20.0%  Cum: 40.0%
   [info] Score 25  Games: 200  Percent: 20.0%  Cum: 20.0%
   [info] Average Score: 19.07
   [info] Average Utility: 48.14
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.9%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.5%
   [info] Score  3  Games: 12  Percent:  1.2%  Cum: 98.6%
   [info] Score  4  Games:  9  Percent:  0.9%  Cum: 97.4%
   [info] Score  5  Games: 15  Percent:  1.5%  Cum: 96.5%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 95.0%
   [info] Score  7  Games: 23  Percent:  2.3%  Cum: 93.9%
   [info] Score  8  Games: 19  Percent:  1.9%  Cum: 91.6%
   [info] Score  9  Games: 19  Percent:  1.9%  Cum: 89.7%
   [info] Score 10  Games: 23  Percent:  2.3%  Cum: 87.8%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 85.5%
   [info] Score 12  Games: 32  Percent:  3.2%  Cum: 83.3%
   [info] Score 13  Games: 36  Percent:  3.6%  Cum: 80.1%
   [info] Score 14  Games: 31  Percent:  3.1%  Cum: 76.5%
   [info] Score 15  Games: 38  Percent:  3.8%  Cum: 73.4%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 69.6%
   [info] Score 17  Games: 51  Percent:  5.1%  Cum: 66.5%
   [info] Score 18  Games: 46  Percent:  4.6%  Cum: 61.4%
   [info] Score 19  Games: 49  Percent:  4.9%  Cum: 56.8%
   [info] Score 20  Games: 52  Percent:  5.2%  Cum: 51.9%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 46.7%
   [info] Score 22  Games: 88  Percent:  8.8%  Cum: 41.9%
   [info] Score 23  Games: 100  Percent: 10.0%  Cum: 33.1%
   [info] Score 24  Games: 164  Percent: 16.4%  Cum: 23.1%
   [info] Score 25  Games: 67  Percent:  6.7%  Cum:  6.7%
   [info] Average Score: 18.064
   [info] Average Utility: 39.478
   [info]
   [info] Time: 980.532206543
   */

}
