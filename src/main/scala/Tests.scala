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
   [info] Score  7  Games: 37  Percent:  3.7%  Cum: 85.3%
   [info] Score  8  Games: 31  Percent:  3.1%  Cum: 81.6%
   [info] Score  9  Games: 24  Percent:  2.4%  Cum: 78.5%
   [info] Score 10  Games: 39  Percent:  3.9%  Cum: 76.1%
   [info] Score 11  Games: 37  Percent:  3.7%  Cum: 72.2%
   [info] Score 12  Games: 30  Percent:  3.0%  Cum: 68.5%
   [info] Score 13  Games: 31  Percent:  3.1%  Cum: 65.5%
   [info] Score 14  Games: 27  Percent:  2.7%  Cum: 62.4%
   [info] Score 15  Games: 23  Percent:  2.3%  Cum: 59.7%
   [info] Score 16  Games: 23  Percent:  2.3%  Cum: 57.4%
   [info] Score 17  Games: 26  Percent:  2.6%  Cum: 55.1%
   [info] Score 18  Games: 27  Percent:  2.7%  Cum: 52.5%
   [info] Score 19  Games: 27  Percent:  2.7%  Cum: 49.8%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 47.1%
   [info] Score 21  Games: 36  Percent:  3.6%  Cum: 45.2%
   [info] Score 22  Games: 30  Percent:  3.0%  Cum: 41.6%
   [info] Score 23  Games: 31  Percent:  3.1%  Cum: 38.6%
   [info] Score 24  Games: 58  Percent:  5.8%  Cum: 35.5%
   [info] Score 25  Games: 297  Percent: 29.7%  Cum: 29.7%
   [info] Average Score: 16.739
   [info] Average Utility: 48.328
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  1  Percent:  0.1%  Cum: 100.0%
   [info] Score  1  Games: 10  Percent:  1.0%  Cum: 99.9%
   [info] Score  2  Games:  5  Percent:  0.5%  Cum: 98.9%
   [info] Score  3  Games: 16  Percent:  1.6%  Cum: 98.4%
   [info] Score  4  Games: 13  Percent:  1.3%  Cum: 96.8%
   [info] Score  5  Games: 13  Percent:  1.3%  Cum: 95.5%
   [info] Score  6  Games: 17  Percent:  1.7%  Cum: 94.2%
   [info] Score  7  Games: 20  Percent:  2.0%  Cum: 92.5%
   [info] Score  8  Games: 16  Percent:  1.6%  Cum: 90.5%
   [info] Score  9  Games: 20  Percent:  2.0%  Cum: 88.9%
   [info] Score 10  Games: 32  Percent:  3.2%  Cum: 86.9%
   [info] Score 11  Games: 21  Percent:  2.1%  Cum: 83.7%
   [info] Score 12  Games: 27  Percent:  2.7%  Cum: 81.6%
   [info] Score 13  Games: 30  Percent:  3.0%  Cum: 78.9%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 75.9%
   [info] Score 15  Games: 27  Percent:  2.7%  Cum: 72.5%
   [info] Score 16  Games: 34  Percent:  3.4%  Cum: 69.8%
   [info] Score 17  Games: 26  Percent:  2.6%  Cum: 66.4%
   [info] Score 18  Games: 43  Percent:  4.3%  Cum: 63.8%
   [info] Score 19  Games: 30  Percent:  3.0%  Cum: 59.5%
   [info] Score 20  Games: 43  Percent:  4.3%  Cum: 56.5%
   [info] Score 21  Games: 43  Percent:  4.3%  Cum: 52.2%
   [info] Score 22  Games: 50  Percent:  5.0%  Cum: 47.9%
   [info] Score 23  Games: 77  Percent:  7.7%  Cum: 42.9%
   [info] Score 24  Games: 225  Percent: 22.5%  Cum: 35.2%
   [info] Score 25  Games: 127  Percent: 12.7%  Cum: 12.7%
   [info] Average Score: 18.42
   [info] Average Utility: 43.19
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  5  Percent:  0.5%  Cum: 99.8%
   [info] Score  2  Games:  9  Percent:  0.9%  Cum: 99.3%
   [info] Score  3  Games: 10  Percent:  1.0%  Cum: 98.4%
   [info] Score  4  Games: 12  Percent:  1.2%  Cum: 97.4%
   [info] Score  5  Games: 16  Percent:  1.6%  Cum: 96.2%
   [info] Score  6  Games: 11  Percent:  1.1%  Cum: 94.6%
   [info] Score  7  Games: 17  Percent:  1.7%  Cum: 93.5%
   [info] Score  8  Games: 19  Percent:  1.9%  Cum: 91.8%
   [info] Score  9  Games: 25  Percent:  2.5%  Cum: 89.9%
   [info] Score 10  Games: 19  Percent:  1.9%  Cum: 87.4%
   [info] Score 11  Games: 32  Percent:  3.2%  Cum: 85.5%
   [info] Score 12  Games: 24  Percent:  2.4%  Cum: 82.3%
   [info] Score 13  Games: 40  Percent:  4.0%  Cum: 79.9%
   [info] Score 14  Games: 39  Percent:  3.9%  Cum: 75.9%
   [info] Score 15  Games: 38  Percent:  3.8%  Cum: 72.0%
   [info] Score 16  Games: 34  Percent:  3.4%  Cum: 68.2%
   [info] Score 17  Games: 40  Percent:  4.0%  Cum: 64.8%
   [info] Score 18  Games: 54  Percent:  5.4%  Cum: 60.8%
   [info] Score 19  Games: 62  Percent:  6.2%  Cum: 55.4%
   [info] Score 20  Games: 63  Percent:  6.3%  Cum: 49.2%
   [info] Score 21  Games: 49  Percent:  4.9%  Cum: 42.9%
   [info] Score 22  Games: 67  Percent:  6.7%  Cum: 38.0%
   [info] Score 23  Games: 124  Percent: 12.4%  Cum: 31.3%
   [info] Score 24  Games: 160  Percent: 16.0%  Cum: 18.9%
   [info] Score 25  Games: 29  Percent:  2.9%  Cum:  2.9%
   [info] Average Score: 17.763
   [info] Average Utility: 36.976
   [info]
   [info] Time: 982.354600615

   */

}
