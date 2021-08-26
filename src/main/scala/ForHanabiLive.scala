package fireflower

import RichImplicits._
import java.io._
import scala.io.StdIn.readLine
import scala.collection.mutable.Map
import play.api.libs.json._

object ForHanabiLive {

  def play(numPlayers: Int) = {
    val games = Map[Int, Game]()
    val players = Map[Int, HeuristicPlayer]()
    val playerIds = Map[Int, PlayerId]()

    while(true) {
      val line = readLine()
      val parsed = Json.parse(line)

      val tableID = parsed("tableID").as[Int]
      val messageType = parsed("messageType").as[String]
      if(messageType == "init") {
        val rules = Rules.Standard(numPlayers)
        val initialPlayer: PlayerId = parsed("options")("startingPlayer").as[Int]
        val game = Game(rules,seed=0L,curPlayer=initialPlayer)
        game.drawInitialCards()

        val myPid: PlayerId = parsed("ourPlayerIndex").as[Int]
        val player = HeuristicPlayer(rules,myPid)
        for(pid <- 0 to (rules.numPlayers-1)) {
          game.hideFor(pid)
        }

        games(tableID) = game
        players(tableID) = player
        playerIds(tableID) = myPid
        System.err.println("Fireflower: Making new game " + tableID + " initialPlayer " + initialPlayer + " me " + myPid)
      }
      else if(messageType == "action") {
        val game = games(tableID)
        val player = players(tableID)
        val myPid = playerIds(tableID)
        val action = parsed("action")
        val actionType = action("type").as[String]

        if(actionType == "draw") {
          val order = action("order").as[Int]
          val color = List(Red,Yellow,Green,Blue,White)(action("cheat_suit").as[Int])
          val rank = action("cheat_rank").as[Int] - 1
          game.seenMap(order) = Card(color,rank)
          System.err.println("Fireflower: game " + tableID + " handled action " + action)
        }
        else if(actionType == "play" || (actionType == "discard" && action("failed").as[Boolean])) {
          val order = action("which")("order").as[Int]
          val seat = action("which")("index").as[Int]
          val color = List(Red,Yellow,Green,Blue,White)(action("which")("suit").as[Int])
          val rank = action("which")("rank").as[Int] - 1
          game.seenMap(order) = Card(color,rank)

          val hid = game.hands(seat).findIdx { cid => cid == order }.get
          val ga = GivePlay(hid)
          val sa = game.seenAction(ga)
          val preGame = Game(game)
          game.doAction(ga)
          player.handleSeenAction(sa, preGame.hiddenFor(myPid), game.hiddenFor(myPid))
          System.err.println("Fireflower: game " + tableID + " handled action " + action)
        }
        else if(actionType == "discard") {
          val order = action("which")("order").as[Int]
          val seat = action("which")("index").as[Int]
          val color = List(Red,Yellow,Green,Blue,White)(action("which")("suit").as[Int])
          val rank = action("which")("rank").as[Int] - 1
          game.seenMap(order) = Card(color,rank)

          val hid = game.hands(seat).findIdx { cid => cid == order }.get
          val ga = GiveDiscard(hid)
          val sa = game.seenAction(ga)
          val preGame = Game(game)
          game.doAction(ga)
          player.handleSeenAction(sa, preGame.hiddenFor(myPid), game.hiddenFor(myPid))
          System.err.println("Fireflower: game " + tableID + " handled action " + action)
        }
        else if(actionType == "clue") {
          val giver = action("giver").as[Int]
          val target = action("target").as[Int]
          val hint_type = action("clue")("type").as[Int]
          val hint_value = action("clue")("value").as[Int]

          val ga = {
            if(hint_type == 0) {
              val color = List(Red,Yellow,Green,Blue,White)(hint_value)
              GiveHint(target,HintColor(color))
            }
            else {
              GiveHint(target,HintNumber(hint_value-1))
            }
          }

          val sa = game.seenAction(ga)
          val preGame = Game(game)
          game.doAction(ga)
          player.handleSeenAction(sa, preGame.hiddenFor(myPid), game.hiddenFor(myPid))
          System.err.println("Fireflower: game " + tableID + " handled action " + action)
        }
      }
      else if(messageType == "yourTurn") {
        val game = games(tableID)
        val player = players(tableID)
        val myPid = playerIds(tableID)
        assert(myPid == game.curPlayer)
        val ga = player.getAction(game.hiddenFor(myPid))
        ga match {
          case GiveDiscard(hid) =>
            val order = game.hands(myPid)(hid)
            println("{\"type\":1,\"target\":" + order + "}")
          case GivePlay(hid) =>
            val order = game.hands(myPid)(hid)
            println("{\"type\":0,\"target\":" + order + "}")
          case GiveHint(pid,hint) =>
            hint match {
              case HintColor(color) =>
                println("{\"type\":2,\"target\":" + pid + ",\"value\":" + color.id + "}")
              case HintNumber(number) =>
                println("{\"type\":3,\"target\":" + pid + ",\"value\":" + (number+1) + "}")
              case _ =>
                assert(false)
            }
        }
        System.err.println("Fireflower: game " + tableID + " sent action " + ga)
      }
      else if(messageType == "finished") {
        games -= tableID
        players -= tableID
        playerIds -= tableID
        System.err.println("Fireflower: game " + tableID + " cleaned up")
      }

    }

  }

  def main(args: Array[String]): Unit = {
    play(numPlayers=2)

  }
}
