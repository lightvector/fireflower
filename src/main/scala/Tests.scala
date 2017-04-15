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
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 96.5%
   [info] Score  6  Games: 25  Percent:  2.5%  Cum: 94.6%
   [info] Score  7  Games: 22  Percent:  2.2%  Cum: 92.1%
   [info] Score  8  Games: 27  Percent:  2.7%  Cum: 89.9%
   [info] Score  9  Games: 29  Percent:  2.9%  Cum: 87.2%
   [info] Score 10  Games: 40  Percent:  4.0%  Cum: 84.3%
   [info] Score 11  Games: 32  Percent:  3.2%  Cum: 80.3%
   [info] Score 12  Games: 28  Percent:  2.8%  Cum: 77.1%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 74.3%
   [info] Score 14  Games: 22  Percent:  2.2%  Cum: 71.3%
   [info] Score 15  Games: 22  Percent:  2.2%  Cum: 69.1%
   [info] Score 16  Games: 15  Percent:  1.5%  Cum: 66.9%
   [info] Score 17  Games: 23  Percent:  2.3%  Cum: 65.4%
   [info] Score 18  Games: 24  Percent:  2.4%  Cum: 63.1%
   [info] Score 19  Games: 24  Percent:  2.4%  Cum: 60.7%
   [info] Score 20  Games: 21  Percent:  2.1%  Cum: 58.3%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 56.2%
   [info] Score 22  Games: 25  Percent:  2.5%  Cum: 53.5%
   [info] Score 23  Games: 39  Percent:  3.9%  Cum: 51.0%
   [info] Score 24  Games: 70  Percent:  7.0%  Cum: 47.1%
   [info] Score 25  Games: 401  Percent: 40.1%  Cum: 40.1%
   [info] Average Score: 18.77
   [info] Average Utility: 57.59
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  2  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  3  Games:  2  Percent:  0.2%  Cum: 99.8%
   [info] Score  4  Games:  5  Percent:  0.5%  Cum: 99.6%
   [info] Score  5  Games:  6  Percent:  0.6%  Cum: 99.1%
   [info] Score  6  Games:  5  Percent:  0.5%  Cum: 98.5%
   [info] Score  7  Games:  7  Percent:  0.7%  Cum: 98.0%
   [info] Score  8  Games:  8  Percent:  0.8%  Cum: 97.3%
   [info] Score  9  Games: 12  Percent:  1.2%  Cum: 96.5%
   [info] Score 10  Games: 18  Percent:  1.8%  Cum: 95.3%
   [info] Score 11  Games: 20  Percent:  2.0%  Cum: 93.5%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 91.5%
   [info] Score 13  Games: 24  Percent:  2.4%  Cum: 89.0%
   [info] Score 14  Games: 24  Percent:  2.4%  Cum: 86.6%
   [info] Score 15  Games: 21  Percent:  2.1%  Cum: 84.2%
   [info] Score 16  Games: 21  Percent:  2.1%  Cum: 82.1%
   [info] Score 17  Games: 37  Percent:  3.7%  Cum: 80.0%
   [info] Score 18  Games: 29  Percent:  2.9%  Cum: 76.3%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 73.4%
   [info] Score 20  Games: 40  Percent:  4.0%  Cum: 70.4%
   [info] Score 21  Games: 42  Percent:  4.2%  Cum: 66.4%
   [info] Score 22  Games: 71  Percent:  7.1%  Cum: 62.2%
   [info] Score 23  Games: 74  Percent:  7.4%  Cum: 55.1%
   [info] Score 24  Games: 224  Percent: 22.4%  Cum: 47.7%
   [info] Score 25  Games: 253  Percent: 25.3%  Cum: 25.3%
   [info] Average Score: 20.678
   [info] Average Utility: 54.006
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  0  Percent:  0.0%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  2  Games:  1  Percent:  0.1%  Cum: 99.8%
   [info] Score  3  Games:  3  Percent:  0.3%  Cum: 99.7%
   [info] Score  4  Games:  6  Percent:  0.6%  Cum: 99.4%
   [info] Score  5  Games:  7  Percent:  0.7%  Cum: 98.8%
   [info] Score  6  Games:  4  Percent:  0.4%  Cum: 98.1%
   [info] Score  7  Games: 11  Percent:  1.1%  Cum: 97.7%
   [info] Score  8  Games: 12  Percent:  1.2%  Cum: 96.6%
   [info] Score  9  Games: 16  Percent:  1.6%  Cum: 95.4%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 93.8%
   [info] Score 11  Games: 25  Percent:  2.5%  Cum: 91.9%
   [info] Score 12  Games: 25  Percent:  2.5%  Cum: 89.4%
   [info] Score 13  Games: 29  Percent:  2.9%  Cum: 86.9%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 84.0%
   [info] Score 15  Games: 31  Percent:  3.1%  Cum: 80.6%
   [info] Score 16  Games: 35  Percent:  3.5%  Cum: 77.5%
   [info] Score 17  Games: 43  Percent:  4.3%  Cum: 74.0%
   [info] Score 18  Games: 49  Percent:  4.9%  Cum: 69.7%
   [info] Score 19  Games: 54  Percent:  5.4%  Cum: 64.8%
   [info] Score 20  Games: 45  Percent:  4.5%  Cum: 59.4%
   [info] Score 21  Games: 65  Percent:  6.5%  Cum: 54.9%
   [info] Score 22  Games: 96  Percent:  9.6%  Cum: 48.4%
   [info] Score 23  Games: 138  Percent: 13.8%  Cum: 38.8%
   [info] Score 24  Games: 168  Percent: 16.8%  Cum: 25.0%
   [info] Score 25  Games: 82  Percent:  8.2%  Cum:  8.2%
   [info] Average Score: 19.328
   [info] Average Utility: 42.756
   [info]
   [info] Time: 1109.961542106

   */

}
