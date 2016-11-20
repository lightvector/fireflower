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
   [info] Score  1  Games:  6  Percent:  0.6%  Cum: 99.8%
   [info] Score  2  Games: 20  Percent:  2.0%  Cum: 99.2%
   [info] Score  3  Games: 22  Percent:  2.2%  Cum: 97.2%
   [info] Score  4  Games: 30  Percent:  3.0%  Cum: 95.0%
   [info] Score  5  Games: 36  Percent:  3.6%  Cum: 92.0%
   [info] Score  6  Games: 31  Percent:  3.1%  Cum: 88.4%
   [info] Score  7  Games: 36  Percent:  3.6%  Cum: 85.3%
   [info] Score  8  Games: 32  Percent:  3.2%  Cum: 81.7%
   [info] Score  9  Games: 23  Percent:  2.3%  Cum: 78.5%
   [info] Score 10  Games: 37  Percent:  3.7%  Cum: 76.2%
   [info] Score 11  Games: 38  Percent:  3.8%  Cum: 72.5%
   [info] Score 12  Games: 32  Percent:  3.2%  Cum: 68.7%
   [info] Score 13  Games: 31  Percent:  3.1%  Cum: 65.5%
   [info] Score 14  Games: 23  Percent:  2.3%  Cum: 62.4%
   [info] Score 15  Games: 25  Percent:  2.5%  Cum: 60.1%
   [info] Score 16  Games: 21  Percent:  2.1%  Cum: 57.6%
   [info] Score 17  Games: 20  Percent:  2.0%  Cum: 55.5%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 53.5%
   [info] Score 19  Games: 21  Percent:  2.1%  Cum: 50.9%
   [info] Score 20  Games: 20  Percent:  2.0%  Cum: 48.8%
   [info] Score 21  Games: 29  Percent:  2.9%  Cum: 46.8%
   [info] Score 22  Games: 28  Percent:  2.8%  Cum: 43.9%
   [info] Score 23  Games: 37  Percent:  3.7%  Cum: 41.1%
   [info] Score 24  Games: 73  Percent:  7.3%  Cum: 37.4%
   [info] Score 25  Games: 301  Percent: 30.1%  Cum: 30.1%
   [info] Average Score: 16.881
   [info] Average Utility: 48.812
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games: 10  Percent:  1.0%  Cum: 99.9%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 98.9%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.4%
   [info] Score  4  Games: 13  Percent:  1.3%  Cum: 96.8%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 95.5%
   [info] Score  6  Games: 16  Percent:  1.6%  Cum: 94.2%
   [info] Score  7  Games: 18  Percent:  1.8%  Cum: 92.6%
   [info] Score  8  Games: 16  Percent:  1.6%  Cum: 90.8%
   [info] Score  9  Games: 20  Percent:  2.0%  Cum: 89.2%
   [info] Score 10  Games: 29  Percent:  2.9%  Cum: 87.2%
   [info] Score 11  Games: 22  Percent:  2.2%  Cum: 84.3%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 82.1%
   [info] Score 13  Games: 27  Percent:  2.7%  Cum: 79.6%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 76.9%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 73.5%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 71.3%
   [info] Score 17  Games: 29  Percent:  2.9%  Cum: 67.5%
   [info] Score 18  Games: 43  Percent:  4.3%  Cum: 64.6%
   [info] Score 19  Games: 39  Percent:  3.9%  Cum: 60.3%
   [info] Score 20  Games: 44  Percent:  4.4%  Cum: 56.4%
   [info] Score 21  Games: 45  Percent:  4.5%  Cum: 52.0%
   [info] Score 22  Games: 51  Percent:  5.1%  Cum: 47.5%
   [info] Score 23  Games: 89  Percent:  8.9%  Cum: 42.4%
   [info] Score 24  Games: 209  Percent: 20.9%  Cum: 33.5%
   [info] Score 25  Games: 126  Percent: 12.6%  Cum: 12.6%
   [info] Average Score: 18.48
   [info] Average Utility: 43.26
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.3%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.4%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 97.4%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 96.2%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 94.6%
   [info] Score  7  Games: 15  Percent:  1.5%  Cum: 93.5%
   [info] Score  8  Games: 20  Percent:  2.0%  Cum: 92.0%
   [info] Score  9  Games: 29  Percent:  2.9%  Cum: 90.0%
   [info] Score 10  Games: 17  Percent:  1.7%  Cum: 87.1%
   [info] Score 11  Games: 31  Percent:  3.1%  Cum: 85.4%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 82.3%
   [info] Score 13  Games: 39  Percent:  3.9%  Cum: 79.8%
   [info] Score 14  Games: 40  Percent:  4.0%  Cum: 75.9%
   [info] Score 15  Games: 42  Percent:  4.2%  Cum: 71.9%
   [info] Score 16  Games: 38  Percent:  3.8%  Cum: 67.7%
   [info] Score 17  Games: 51  Percent:  5.1%  Cum: 63.9%
   [info] Score 18  Games: 55  Percent:  5.5%  Cum: 58.8%
   [info] Score 19  Games: 65  Percent:  6.5%  Cum: 53.3%
   [info] Score 20  Games: 59  Percent:  5.9%  Cum: 46.8%
   [info] Score 21  Games: 48  Percent:  4.8%  Cum: 40.9%
   [info] Score 22  Games: 72  Percent:  7.2%  Cum: 36.1%
   [info] Score 23  Games: 107  Percent: 10.7%  Cum: 28.9%
   [info] Score 24  Games: 154  Percent: 15.4%  Cum: 18.2%
   [info] Score 25  Games: 28  Percent:  2.8%  Cum:  2.8%
   [info] Average Score: 17.61
   [info] Average Utility: 36.62
   [info]
   [info] Time: 979.559768142

   */

}
