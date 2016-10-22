package fireflower

object PlayerTests {

  def main(args: Array[String]): Unit = {
    runTests(prefix="",numGames=1000)
    // runTests(prefix="Quick2",numGames=100)
  }

  def runTests(prefix:String, numGames: Int): Unit = {
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

    println(name2p + ":")
    printScoreSummary(rules2p,games2p)
    println("")
    println(name3p + ":")
    printScoreSummary(rules3p,games3p)
    println("")
    println(name4p + ":")
    printScoreSummary(rules4p,games4p)
    println("")

  }

  def printScoreSummary(rules: Rules, games: List[Game]) = {
    val scoreTable = (0 to rules.maxScore).map { score =>
      (score,games.count(game => game.numPlayed == score))
    }
    val numGames = games.length
    scoreTable.foreach { case (score,count) =>
      println("Score %2d  Games: %2d  Percent: %4.1f%%".format(score, count, count.toDouble * 100.0 / numGames))
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
   [info] Score  0  Games: 14  Percent:  1.4%
   [info] Score  1  Games:  5  Percent:  0.5%
   [info] Score  2  Games: 14  Percent:  1.4%
   [info] Score  3  Games: 31  Percent:  3.1%
   [info] Score  4  Games: 25  Percent:  2.5%
   [info] Score  5  Games: 24  Percent:  2.4%
   [info] Score  6  Games: 46  Percent:  4.6%
   [info] Score  7  Games: 38  Percent:  3.8%
   [info] Score  8  Games: 52  Percent:  5.2%
   [info] Score  9  Games: 71  Percent:  7.1%
   [info] Score 10  Games: 55  Percent:  5.5%
   [info] Score 11  Games: 51  Percent:  5.1%
   [info] Score 12  Games: 59  Percent:  5.9%
   [info] Score 13  Games: 43  Percent:  4.3%
   [info] Score 14  Games: 65  Percent:  6.5%
   [info] Score 15  Games: 51  Percent:  5.1%
   [info] Score 16  Games: 38  Percent:  3.8%
   [info] Score 17  Games: 38  Percent:  3.8%
   [info] Score 18  Games: 31  Percent:  3.1%
   [info] Score 19  Games: 26  Percent:  2.6%
   [info] Score 20  Games: 18  Percent:  1.8%
   [info] Score 21  Games: 26  Percent:  2.6%
   [info] Score 22  Games: 24  Percent:  2.4%
   [info] Score 23  Games: 31  Percent:  3.1%
   [info] Score 24  Games: 89  Percent:  8.9%
   [info] Score 25  Games: 35  Percent:  3.5%
   [info] Average Score: 13.46
   [info] Average Utility: 28.67
   [info]
   [info] HeuristicStandard3P:
   [info] Score  0  Games:  2  Percent:  0.2%
   [info] Score  1  Games:  7  Percent:  0.7%
   [info] Score  2  Games: 15  Percent:  1.5%
   [info] Score  3  Games: 15  Percent:  1.5%
   [info] Score  4  Games: 29  Percent:  2.9%
   [info] Score  5  Games: 27  Percent:  2.7%
   [info] Score  6  Games: 27  Percent:  2.7%
   [info] Score  7  Games: 40  Percent:  4.0%
   [info] Score  8  Games: 40  Percent:  4.0%
   [info] Score  9  Games: 49  Percent:  4.9%
   [info] Score 10  Games: 57  Percent:  5.7%
   [info] Score 11  Games: 64  Percent:  6.4%
   [info] Score 12  Games: 59  Percent:  5.9%
   [info] Score 13  Games: 63  Percent:  6.3%
   [info] Score 14  Games: 74  Percent:  7.4%
   [info] Score 15  Games: 69  Percent:  6.9%
   [info] Score 16  Games: 75  Percent:  7.5%
   [info] Score 17  Games: 65  Percent:  6.5%
   [info] Score 18  Games: 60  Percent:  6.0%
   [info] Score 19  Games: 42  Percent:  4.2%
   [info] Score 20  Games: 35  Percent:  3.5%
   [info] Score 21  Games: 28  Percent:  2.8%
   [info] Score 22  Games: 21  Percent:  2.1%
   [info] Score 23  Games: 28  Percent:  2.8%
   [info] Score 24  Games:  7  Percent:  0.7%
   [info] Score 25  Games:  2  Percent:  0.2%
   [info] Average Score: 13.203
   [info] Average Utility: 26.506
   [info]
   [info] HeuristicStandard4P:
   [info] Score  0  Games:  1  Percent:  0.1%
   [info] Score  1  Games:  2  Percent:  0.2%
   [info] Score  2  Games: 16  Percent:  1.6%
   [info] Score  3  Games: 18  Percent:  1.8%
   [info] Score  4  Games: 19  Percent:  1.9%
   [info] Score  5  Games: 13  Percent:  1.3%
   [info] Score  6  Games: 34  Percent:  3.4%
   [info] Score  7  Games: 32  Percent:  3.2%
   [info] Score  8  Games: 53  Percent:  5.3%
   [info] Score  9  Games: 62  Percent:  6.2%
   [info] Score 10  Games: 59  Percent:  5.9%
   [info] Score 11  Games: 65  Percent:  6.5%
   [info] Score 12  Games: 70  Percent:  7.0%
   [info] Score 13  Games: 69  Percent:  6.9%
   [info] Score 14  Games: 93  Percent:  9.3%
   [info] Score 15  Games: 77  Percent:  7.7%
   [info] Score 16  Games: 73  Percent:  7.3%
   [info] Score 17  Games: 60  Percent:  6.0%
   [info] Score 18  Games: 60  Percent:  6.0%
   [info] Score 19  Games: 38  Percent:  3.8%
   [info] Score 20  Games: 36  Percent:  3.6%
   [info] Score 21  Games: 20  Percent:  2.0%
   [info] Score 22  Games: 17  Percent:  1.7%
   [info] Score 23  Games: 11  Percent:  1.1%
   [info] Score 24  Games:  2  Percent:  0.2%
   [info] Score 25  Games:  0  Percent:  0.0%
   [info] Average Score: 12.943
   [info] Average Utility: 25.886

   */

}
