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
   [info] Score  1  Games:  8  Percent:  0.8%  Cum: 100.0%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 99.2%
   [info] Score  3  Games:  6  Percent:  0.6%  Cum: 98.7%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 98.1%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 97.4%
   [info] Score  6  Games: 12  Percent:  1.2%  Cum: 96.6%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 95.4%
   [info] Score  8  Games: 16  Percent:  1.6%  Cum: 93.2%
   [info] Score  9  Games: 23  Percent:  2.3%  Cum: 91.6%
   [info] Score 10  Games: 24  Percent:  2.4%  Cum: 89.3%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 86.9%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 84.8%
   [info] Score 13  Games: 36  Percent:  3.6%  Cum: 82.3%
   [info] Score 14  Games: 38  Percent:  3.8%  Cum: 78.7%
   [info] Score 15  Games: 24  Percent:  2.4%  Cum: 74.9%
   [info] Score 16  Games: 45  Percent:  4.5%  Cum: 72.5%
   [info] Score 17  Games: 38  Percent:  3.8%  Cum: 68.0%
   [info] Score 18  Games: 32  Percent:  3.2%  Cum: 64.2%
   [info] Score 19  Games: 51  Percent:  5.1%  Cum: 61.0%
   [info] Score 20  Games: 30  Percent:  3.0%  Cum: 55.9%
   [info] Score 21  Games: 51  Percent:  5.1%  Cum: 52.9%
   [info] Score 22  Games: 57  Percent:  5.7%  Cum: 47.8%
   [info] Score 23  Games: 88  Percent:  8.8%  Cum: 42.1%
   [info] Score 24  Games: 211  Percent: 21.1%  Cum: 33.3%
   [info] Score 25  Games: 122  Percent: 12.2%  Cum: 12.2%
   [info] Average Score: 18.77
   [info] Average Utility: 43.64
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  4  Percent:  0.4%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 99.4%
   [info] Score  3  Games:  5  Percent:  0.5%  Cum: 98.8%
   [info] Score  4  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  5  Games:  8  Percent:  0.8%  Cum: 97.5%
   [info] Score  6  Games: 18  Percent:  1.8%  Cum: 96.7%
   [info] Score  7  Games: 21  Percent:  2.1%  Cum: 94.9%
   [info] Score  8  Games: 18  Percent:  1.8%  Cum: 92.8%
   [info] Score  9  Games: 27  Percent:  2.7%  Cum: 91.0%
   [info] Score 10  Games: 27  Percent:  2.7%  Cum: 88.3%
   [info] Score 11  Games: 30  Percent:  3.0%  Cum: 85.6%
   [info] Score 12  Games: 36  Percent:  3.6%  Cum: 82.6%
   [info] Score 13  Games: 35  Percent:  3.5%  Cum: 79.0%
   [info] Score 14  Games: 36  Percent:  3.6%  Cum: 75.5%
   [info] Score 15  Games: 64  Percent:  6.4%  Cum: 71.9%
   [info] Score 16  Games: 47  Percent:  4.7%  Cum: 65.5%
   [info] Score 17  Games: 53  Percent:  5.3%  Cum: 60.8%
   [info] Score 18  Games: 52  Percent:  5.2%  Cum: 55.5%
   [info] Score 19  Games: 61  Percent:  6.1%  Cum: 50.3%
   [info] Score 20  Games: 48  Percent:  4.8%  Cum: 44.2%
   [info] Score 21  Games: 55  Percent:  5.5%  Cum: 39.4%
   [info] Score 22  Games: 61  Percent:  6.1%  Cum: 33.9%
   [info] Score 23  Games: 108  Percent: 10.8%  Cum: 27.8%
   [info] Score 24  Games: 131  Percent: 13.1%  Cum: 17.0%
   [info] Score 25  Games: 39  Percent:  3.9%  Cum:  3.9%
   [info] Average Score: 17.504
   [info] Average Utility: 36.958
   [info]
   [info] Time: 953.643278815

   */

}
