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

    runTests(prefix="",salt="b",numGames=numGames, numPlayers=numPlayers)
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
   [info] Score  1  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  2  Games:  4  Percent:  0.4%  Cum: 99.9%
   [info] Score  3  Games:  9  Percent:  0.9%  Cum: 99.5%
   [info] Score  4  Games: 21  Percent:  2.1%  Cum: 98.6%
   [info] Score  5  Games: 20  Percent:  2.0%  Cum: 96.5%
   [info] Score  6  Games: 25  Percent:  2.5%  Cum: 94.5%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 92.0%
   [info] Score  8  Games: 29  Percent:  2.9%  Cum: 89.8%
   [info] Score  9  Games: 31  Percent:  3.1%  Cum: 86.9%
   [info] Score 10  Games: 43  Percent:  4.3%  Cum: 83.8%
   [info] Score 11  Games: 31  Percent:  3.1%  Cum: 79.5%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 76.4%
   [info] Score 13  Games: 29  Percent:  2.9%  Cum: 73.9%
   [info] Score 14  Games: 23  Percent:  2.3%  Cum: 71.0%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 68.7%
   [info] Score 16  Games: 20  Percent:  2.0%  Cum: 66.6%
   [info] Score 17  Games: 25  Percent:  2.5%  Cum: 64.6%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 62.1%
   [info] Score 19  Games: 28  Percent:  2.8%  Cum: 59.4%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 56.6%
   [info] Score 21  Games: 26  Percent:  2.6%  Cum: 54.7%
   [info] Score 22  Games: 24  Percent:  2.4%  Cum: 52.1%
   [info] Score 23  Games: 36  Percent:  3.6%  Cum: 49.7%
   [info] Score 24  Games: 71  Percent:  7.1%  Cum: 46.1%
   [info] Score 25  Games: 390  Percent: 39.0%  Cum: 39.0%
   [info] Average Score: 18.619
   [info] Average Utility: 56.738
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.6%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 99.1%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.5%
   [info] Score  7  Games:  6  Percent:  0.6%  Cum: 98.0%
   [info] Score  8  Games:  8  Percent:  0.8%  Cum: 97.4%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 96.6%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 95.4%
   [info] Score 11  Games: 20  Percent:  2.0%  Cum: 93.5%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 91.5%
   [info] Score 13  Games: 28  Percent:  2.8%  Cum: 89.0%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 86.2%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 84.0%
   [info] Score 16  Games: 21  Percent:  2.1%  Cum: 81.8%
   [info] Score 17  Games: 36  Percent:  3.6%  Cum: 79.7%
   [info] Score 18  Games: 30  Percent:  3.0%  Cum: 76.1%
   [info] Score 19  Games: 38  Percent:  3.8%  Cum: 73.1%
   [info] Score 20  Games: 38  Percent:  3.8%  Cum: 69.3%
   [info] Score 21  Games: 43  Percent:  4.3%  Cum: 65.5%
   [info] Score 22  Games: 72  Percent:  7.2%  Cum: 61.2%
   [info] Score 23  Games: 71  Percent:  7.1%  Cum: 54.0%
   [info] Score 24  Games: 222  Percent: 22.2%  Cum: 46.9%
   [info] Score 25  Games: 247  Percent: 24.7%  Cum: 24.7%
   [info] Average Score: 20.609
   [info] Average Utility: 53.568
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.8%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  4  Games:  7  Percent:  0.7%  Cum: 99.4%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 98.7%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.0%
   [info] Score  7  Games: 12  Percent:  1.2%  Cum: 97.6%
   [info] Score  8  Games: 13  Percent:  1.3%  Cum: 96.4%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 95.1%
   [info] Score 10  Games: 20  Percent:  2.0%  Cum: 93.5%
   [info] Score 11  Games: 25  Percent:  2.5%  Cum: 91.5%
   [info] Score 12  Games: 26  Percent:  2.6%  Cum: 89.0%
   [info] Score 13  Games: 29  Percent:  2.9%  Cum: 86.4%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 83.5%
   [info] Score 15  Games: 33  Percent:  3.3%  Cum: 80.1%
   [info] Score 16  Games: 36  Percent:  3.6%  Cum: 76.8%
   [info] Score 17  Games: 43  Percent:  4.3%  Cum: 73.2%
   [info] Score 18  Games: 51  Percent:  5.1%  Cum: 68.9%
   [info] Score 19  Games: 54  Percent:  5.4%  Cum: 63.8%
   [info] Score 20  Games: 47  Percent:  4.7%  Cum: 58.4%
   [info] Score 21  Games: 61  Percent:  6.1%  Cum: 53.7%
   [info] Score 22  Games: 90  Percent:  9.0%  Cum: 47.6%
   [info] Score 23  Games: 137  Percent: 13.7%  Cum: 38.6%
   [info] Score 24  Games: 170  Percent: 17.0%  Cum: 24.9%
   [info] Score 25  Games: 79  Percent:  7.9%  Cum:  7.9%
   [info] Average Score: 19.225
   [info] Average Utility: 42.4
   [info]
   [info] Time: 1093.947876451

   */

}
