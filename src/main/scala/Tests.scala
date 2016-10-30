package fireflower

object PlayerTests {

  def main(args: Array[String]): Unit = {
    val numGames = {
      if(args.length >= 1)
        args(0).toInt
      else
        1000
    }
    println("NumGames=" + numGames)

    runTests(prefix="",salt="a",numGames=numGames)
  }

  def runTests(prefix: String, salt: String, numGames: Int): Unit = {
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
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.8%
   [info] Score  2  Games: 11  Percent:  1.1%  Cum: 99.4%
   [info] Score  3  Games: 22  Percent:  2.2%  Cum: 98.3%
   [info] Score  4  Games: 23  Percent:  2.3%  Cum: 96.1%
   [info] Score  5  Games: 43  Percent:  4.3%  Cum: 93.8%
   [info] Score  6  Games: 37  Percent:  3.7%  Cum: 89.5%
   [info] Score  7  Games: 37  Percent:  3.7%  Cum: 85.8%
   [info] Score  8  Games: 37  Percent:  3.7%  Cum: 82.1%
   [info] Score  9  Games: 43  Percent:  4.3%  Cum: 78.4%
   [info] Score 10  Games: 22  Percent:  2.2%  Cum: 74.1%
   [info] Score 11  Games: 40  Percent:  4.0%  Cum: 71.9%
   [info] Score 12  Games: 41  Percent:  4.1%  Cum: 67.9%
   [info] Score 13  Games: 29  Percent:  2.9%  Cum: 63.8%
   [info] Score 14  Games: 33  Percent:  3.3%  Cum: 60.9%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 57.6%
   [info] Score 16  Games: 31  Percent:  3.1%  Cum: 55.3%
   [info] Score 17  Games: 30  Percent:  3.0%  Cum: 52.2%
   [info] Score 18  Games: 20  Percent:  2.0%  Cum: 49.2%
   [info] Score 19  Games: 24  Percent:  2.4%  Cum: 47.2%
   [info] Score 20  Games: 22  Percent:  2.2%  Cum: 44.8%
   [info] Score 21  Games: 39  Percent:  3.9%  Cum: 42.6%
   [info] Score 22  Games: 25  Percent:  2.5%  Cum: 38.7%
   [info] Score 23  Games: 19  Percent:  1.9%  Cum: 36.2%
   [info] Score 24  Games: 69  Percent:  6.9%  Cum: 34.3%
   [info] Score 25  Games: 274  Percent: 27.4%  Cum: 27.4%
   [info] Average Score: 16.473
   [info] Average Utility: 46.646
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games: 11  Percent:  1.1%  Cum: 100.0%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 98.9%
   [info] Score  3  Games:  7  Percent:  0.7%  Cum: 98.3%
   [info] Score  4  Games: 20  Percent:  2.0%  Cum: 97.6%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 95.6%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 94.0%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 92.6%
   [info] Score  8  Games: 20  Percent:  2.0%  Cum: 90.6%
   [info] Score  9  Games: 31  Percent:  3.1%  Cum: 88.6%
   [info] Score 10  Games: 34  Percent:  3.4%  Cum: 85.5%
   [info] Score 11  Games: 29  Percent:  2.9%  Cum: 82.1%
   [info] Score 12  Games: 32  Percent:  3.2%  Cum: 79.2%
   [info] Score 13  Games: 33  Percent:  3.3%  Cum: 76.0%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 72.7%
   [info] Score 15  Games: 43  Percent:  4.3%  Cum: 69.3%
   [info] Score 16  Games: 50  Percent:  5.0%  Cum: 65.0%
   [info] Score 17  Games: 53  Percent:  5.3%  Cum: 60.0%
   [info] Score 18  Games: 52  Percent:  5.2%  Cum: 54.7%
   [info] Score 19  Games: 56  Percent:  5.6%  Cum: 49.5%
   [info] Score 20  Games: 57  Percent:  5.7%  Cum: 43.9%
   [info] Score 21  Games: 52  Percent:  5.2%  Cum: 38.2%
   [info] Score 22  Games: 68  Percent:  6.8%  Cum: 33.0%
   [info] Score 23  Games: 91  Percent:  9.1%  Cum: 26.2%
   [info] Score 24  Games: 115  Percent: 11.5%  Cum: 17.1%
   [info] Score 25  Games: 56  Percent:  5.6%  Cum:  5.6%
   [info] Average Score: 17.142
   [info] Average Utility: 37.084
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.9%
   [info] Score  2  Games:  3  Percent:  0.3%  Cum: 99.3%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 99.0%
   [info] Score  4  Games: 19  Percent:  1.9%  Cum: 98.2%
   [info] Score  5  Games: 21  Percent:  2.1%  Cum: 96.3%
   [info] Score  6  Games: 20  Percent:  2.0%  Cum: 94.2%
   [info] Score  7  Games: 19  Percent:  1.9%  Cum: 92.2%
   [info] Score  8  Games: 26  Percent:  2.6%  Cum: 90.3%
   [info] Score  9  Games: 46  Percent:  4.6%  Cum: 87.7%
   [info] Score 10  Games: 35  Percent:  3.5%  Cum: 83.1%
   [info] Score 11  Games: 51  Percent:  5.1%  Cum: 79.6%
   [info] Score 12  Games: 55  Percent:  5.5%  Cum: 74.5%
   [info] Score 13  Games: 59  Percent:  5.9%  Cum: 69.0%
   [info] Score 14  Games: 45  Percent:  4.5%  Cum: 63.1%
   [info] Score 15  Games: 59  Percent:  5.9%  Cum: 58.6%
   [info] Score 16  Games: 69  Percent:  6.9%  Cum: 52.7%
   [info] Score 17  Games: 58  Percent:  5.8%  Cum: 45.8%
   [info] Score 18  Games: 58  Percent:  5.8%  Cum: 40.0%
   [info] Score 19  Games: 64  Percent:  6.4%  Cum: 34.2%
   [info] Score 20  Games: 54  Percent:  5.4%  Cum: 27.8%
   [info] Score 21  Games: 53  Percent:  5.3%  Cum: 22.4%
   [info] Score 22  Games: 48  Percent:  4.8%  Cum: 17.1%
   [info] Score 23  Games: 71  Percent:  7.1%  Cum: 12.3%
   [info] Score 24  Games: 40  Percent:  4.0%  Cum:  5.2%
   [info] Score 25  Games: 12  Percent:  1.2%  Cum:  1.2%
   [info] Average Score: 15.437
   [info] Average Utility: 31.474
   [info]
   [info] Time: 611.274255008

   */

}
