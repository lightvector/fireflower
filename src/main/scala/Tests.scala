package fireflower

object PlayerTests {

  def main(args: Array[String]): Unit = {
    runTests(prefix="",numGames=1000)
    // runTests(prefix="Quick2",numGames=100)
  }

  def runTests(prefix:String, numGames: Int): Unit = {
    val start = System.nanoTime()
    val rules2p = Rules.Standard(numPlayers=2)
    val rules3p = Rules.Standard(numPlayers=3)
    val rules4p = Rules.Standard(numPlayers=4)

    val name2p = prefix + "HeuristicStandard2P"
    val games2p = {
      Sim.runMulti(
        name = name2p,
        rules = rules2p,
        numGames,
        runSeed = RandUtils.sha256Long(name2p),
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
        runSeed = RandUtils.sha256Long(name3p),
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
        runSeed = RandUtils.sha256Long(name4p),
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
   [info] Score  0  Games: 14  Percent:  1.4%  Cum: 100.0%
   [info] Score  1  Games:  8  Percent:  0.8%  Cum: 98.6%
   [info] Score  2  Games: 16  Percent:  1.6%  Cum: 97.8%
   [info] Score  3  Games: 21  Percent:  2.1%  Cum: 96.2%
   [info] Score  4  Games: 36  Percent:  3.6%  Cum: 94.1%
   [info] Score  5  Games: 25  Percent:  2.5%  Cum: 90.5%
   [info] Score  6  Games: 43  Percent:  4.3%  Cum: 88.0%
   [info] Score  7  Games: 38  Percent:  3.8%  Cum: 83.7%
   [info] Score  8  Games: 32  Percent:  3.2%  Cum: 79.9%
   [info] Score  9  Games: 52  Percent:  5.2%  Cum: 76.7%
   [info] Score 10  Games: 33  Percent:  3.3%  Cum: 71.5%
   [info] Score 11  Games: 44  Percent:  4.4%  Cum: 68.2%
   [info] Score 12  Games: 37  Percent:  3.7%  Cum: 63.8%
   [info] Score 13  Games: 18  Percent:  1.8%  Cum: 60.1%
   [info] Score 14  Games: 34  Percent:  3.4%  Cum: 58.3%
   [info] Score 15  Games: 26  Percent:  2.6%  Cum: 54.9%
   [info] Score 16  Games: 25  Percent:  2.5%  Cum: 52.3%
   [info] Score 17  Games: 27  Percent:  2.7%  Cum: 49.8%
   [info] Score 18  Games: 26  Percent:  2.6%  Cum: 47.1%
   [info] Score 19  Games: 34  Percent:  3.4%  Cum: 44.5%
   [info] Score 20  Games: 19  Percent:  1.9%  Cum: 41.1%
   [info] Score 21  Games: 27  Percent:  2.7%  Cum: 39.2%
   [info] Score 22  Games: 30  Percent:  3.0%  Cum: 36.5%
   [info] Score 23  Games: 23  Percent:  2.3%  Cum: 33.5%
   [info] Score 24  Games: 48  Percent:  4.8%  Cum: 31.2%
   [info] Score 25  Games: 264  Percent: 26.4%  Cum: 26.4%
   [info] Average Score: 15.839
   [info] Average Utility: 44.878
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%  Cum: 100.0%
   [info] Score  1  Games:  9  Percent:  0.9%  Cum: 99.8%
   [info] Score  2  Games:  6  Percent:  0.6%  Cum: 98.9%
   [info] Score  3  Games:  8  Percent:  0.8%  Cum: 98.3%
   [info] Score  4  Games: 18  Percent:  1.8%  Cum: 97.5%
   [info] Score  5  Games: 22  Percent:  2.2%  Cum: 95.7%
   [info] Score  6  Games: 20  Percent:  2.0%  Cum: 93.5%
   [info] Score  7  Games: 25  Percent:  2.5%  Cum: 91.5%
   [info] Score  8  Games: 15  Percent:  1.5%  Cum: 89.0%
   [info] Score  9  Games: 35  Percent:  3.5%  Cum: 87.5%
   [info] Score 10  Games: 31  Percent:  3.1%  Cum: 84.0%
   [info] Score 11  Games: 37  Percent:  3.7%  Cum: 80.9%
   [info] Score 12  Games: 35  Percent:  3.5%  Cum: 77.2%
   [info] Score 13  Games: 44  Percent:  4.4%  Cum: 73.7%
   [info] Score 14  Games: 46  Percent:  4.6%  Cum: 69.3%
   [info] Score 15  Games: 41  Percent:  4.1%  Cum: 64.7%
   [info] Score 16  Games: 53  Percent:  5.3%  Cum: 60.6%
   [info] Score 17  Games: 60  Percent:  6.0%  Cum: 55.3%
   [info] Score 18  Games: 61  Percent:  6.1%  Cum: 49.3%
   [info] Score 19  Games: 50  Percent:  5.0%  Cum: 43.2%
   [info] Score 20  Games: 51  Percent:  5.1%  Cum: 38.2%
   [info] Score 21  Games: 59  Percent:  5.9%  Cum: 33.1%
   [info] Score 22  Games: 58  Percent:  5.8%  Cum: 27.2%
   [info] Score 23  Games: 77  Percent:  7.7%  Cum: 21.4%
   [info] Score 24  Games: 93  Percent:  9.3%  Cum: 13.7%
   [info] Score 25  Games: 44  Percent:  4.4%  Cum:  4.4%
   [info] Average Score: 16.479
   [info] Average Utility: 35.158
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  3  Percent:  0.3%  Cum: 100.0%
   [info] Score  1  Games:  2  Percent:  0.2%  Cum: 99.7%
   [info] Score  2  Games:  7  Percent:  0.7%  Cum: 99.5%
   [info] Score  3  Games:  9  Percent:  0.9%  Cum: 98.8%
   [info] Score  4  Games:  9  Percent:  0.9%  Cum: 97.9%
   [info] Score  5  Games: 19  Percent:  1.9%  Cum: 97.0%
   [info] Score  6  Games: 14  Percent:  1.4%  Cum: 95.1%
   [info] Score  7  Games: 27  Percent:  2.7%  Cum: 93.7%
   [info] Score  8  Games: 37  Percent:  3.7%  Cum: 91.0%
   [info] Score  9  Games: 51  Percent:  5.1%  Cum: 87.3%
   [info] Score 10  Games: 46  Percent:  4.6%  Cum: 82.2%
   [info] Score 11  Games: 55  Percent:  5.5%  Cum: 77.6%
   [info] Score 12  Games: 57  Percent:  5.7%  Cum: 72.1%
   [info] Score 13  Games: 63  Percent:  6.3%  Cum: 66.4%
   [info] Score 14  Games: 76  Percent:  7.6%  Cum: 60.1%
   [info] Score 15  Games: 68  Percent:  6.8%  Cum: 52.5%
   [info] Score 16  Games: 61  Percent:  6.1%  Cum: 45.7%
   [info] Score 17  Games: 59  Percent:  5.9%  Cum: 39.6%
   [info] Score 18  Games: 75  Percent:  7.5%  Cum: 33.7%
   [info] Score 19  Games: 57  Percent:  5.7%  Cum: 26.2%
   [info] Score 20  Games: 35  Percent:  3.5%  Cum: 20.5%
   [info] Score 21  Games: 40  Percent:  4.0%  Cum: 17.0%
   [info] Score 22  Games: 52  Percent:  5.2%  Cum: 13.0%
   [info] Score 23  Games: 48  Percent:  4.8%  Cum:  7.8%
   [info] Score 24  Games: 25  Percent:  2.5%  Cum:  3.0%
   [info] Score 25  Games:  5  Percent:  0.5%  Cum:  0.5%
   [info] Average Score: 14.779
   [info] Average Utility: 29.808
   [info]
   [info] Time: 846.886755951

   */

}
