package fireflower

import RichImplicits._

object Interactive {
  def inputCard(prompt:String): Card = {
    print(prompt + ": ")
    //TODO fix this
    assertUnreachable()
  }

  def inputAction(prompt:String, game: Game): SeenAction = {
    print(prompt + ": ")
    //TODO fix this
    assertUnreachable()
  }

  def playInRealWorld(numPlayers: Int, myPid: PlayerId): Unit = {
    val rules = Rules.Standard(numPlayers)
    val game = Game(rules,seed=0L)
    game.drawInitialCards()

    val player = HeuristicPlayer(rules,myPid)
    for(pid <- 0 to (rules.numPlayers-1)) {
      game.hideFor(pid)
    }

    {
      println("Enter cards players drew, starting with the person to your left, from newest to oldest.")
      var pid = (myPid+1) % rules.numPlayers
      while(pid != myPid) {
        for(hid <- 0 to (game.hands(pid).length-1)) {
          game.seenMap(game.hands(pid)(hid)) = inputCard("Player " + pid + "HandPos " + (hid+1))
        }
        pid = (pid+1) % rules.numPlayers
      }
    }
    player.handleGameStart(Game(game))

    var pid = 0
    while(!game.isDone()) {
      if(pid == myPid) {
        val ga = player.getAction(Game(game))
        ga match {
          case GiveDiscard(hid) =>
            println("Discard #" + (hid+1))
            val card = inputCard("Discard was a")
            game.seenMap(game.hands(pid)(hid)) = card
            val sa = game.seenAction(ga)
            game.doAction(ga)
            player.handleSeenAction(sa, Game(game))
          case GivePlay(hid) =>
            println("Play #" + (hid+1))
            val card = inputCard("Play was a")
            game.seenMap(game.hands(pid)(hid)) = card
            val sa = game.seenAction(ga)
            game.doAction(ga)
            player.handleSeenAction(sa, Game(game))
          case GiveHint(pid,hint) =>
            val sa = game.seenAction(ga) match {
              case (_: SeenDiscard) => assertUnreachable()
              case (_: SeenPlay) => assertUnreachable()
              case (_: SeenBomb) => assertUnreachable()
              case (x: SeenHint) => x
            }
            game.doAction(ga)
            player.handleSeenAction(sa, Game(game))

            val appliedTo = sa.appliedTo.zipWithIndex.flatMap { case (b,hid) =>
              if(b) Some(hid.toString) else None
            }.mkString("")
            val hintString = sa.hint match {
              case HintColor(color) =>
                color.toAnsiColorCode() + color.toString() + Color.ansiResetColor
              case HintNumber(number) =>
                (number+1).toString()
              case HintSameColor =>
                "a color"
              case HintSameNumber =>
                "a number"
              case HintSame =>
                "something"
              case UnknownHint =>
                "unknown"
            }
            println("Hint player " + pid + " cards #" + appliedTo + " are " + hintString)
        }
      }
      else {
        val sa = inputAction("Player " + pid + " action", game)
        //Hacky - convert sa to a ga, and the precise hint doesn't matter because the
        //game's state update doesn't depend on it!
        var shouldDraw = game.finalTurnsLeft < 0
        val ga: GiveAction = sa match {
          case SeenDiscard(hid,_) => GiveDiscard(hid)
          case SeenPlay(hid,_) => GivePlay(hid)
          case SeenBomb(hid,_) => GivePlay(hid)
          case SeenHint(pid,_,_) => shouldDraw=false; GiveHint(pid,HintNumber(0))
        }
        game.doAction(ga)
        if(shouldDraw) {
          val card = inputCard("Drawn card was")
          game.seenMap(game.hands(pid)(0)) = card
        }
        player.handleSeenAction(sa, Game(game))
      }
      pid = (pid+1) % rules.numPlayers
    }
  }

  def main(args: Array[String]): Unit = {
    playInRealWorld(numPlayers=2, myPid=0)

  }
}
