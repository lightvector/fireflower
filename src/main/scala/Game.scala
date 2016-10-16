package fireflower

import RichImplicits._

object Game {
  def apply(rules: Rules, seed: Long): Game = {
    val cardMap = CardMap(rules,Rand(seed))
    new Game(
      rules = rules,
      turnNumber = 0,
      numHints = rules.initialHints,
      numBombs = 0,
      numPlayed = 0,
      numDiscarded = 0,
      cardMap = cardMap,
      played = List(),
      discarded = List(),
      deck = (0 to (rules.deckSize-1)).toList,
      curPlayer = 0,
      finalTurnsLeft = -1,
      hands = Array.fill(rules.numPlayers)(Hand(rules.handSize)),
      nextPlayable = Array.fill(rules.maxColorId+1)(1),
      revHistory = List()
    )
  }

  def apply(that: Game): Game = {
    new Game(
      rules = that.rules,
      turnNumber = that.turnNumber,
      numHints = that.numHints,
      numBombs = that.numBombs,
      numPlayed = that.numPlayed,
      numDiscarded = that.numDiscarded,
      cardMap = CardMap(that.cardMap),
      played = that.played,
      discarded = that.discarded,
      deck = that.deck,
      curPlayer = that.curPlayer,
      finalTurnsLeft = that.finalTurnsLeft,
      hands = that.hands.map { hand => Hand(hand) },
      nextPlayable = that.nextPlayable.clone(),
      revHistory = that.revHistory
    )
  }

}

class Game private (
  val rules: Rules,
  var turnNumber: Int,
  var numHints: Int,
  var numBombs: Int,
  var numPlayed: Int,
  var numDiscarded: Int,
  var cardMap: CardMap,
  var played: List[CardId],
  var discarded: List[CardId],
  var deck: List[CardId],
  var curPlayer: PlayerId,
  var finalTurnsLeft: Int,
  val hands: Array[Hand],
  val nextPlayable: Array[Int], //Indexed by ColorId
  val revHistory: List[SeenAction]
) {

  def isLegal(ga: GiveAction): Boolean = {
    ga match {
      case GiveDiscard(hid) =>
        numHints < rules.maxHints && hid >= 0 && hid < hands(curPlayer).numCards
      case GivePlay(hid) =>
        hid >= 0 && hid < hands(curPlayer).numCards
      case GiveHint(pid,hint) =>
        numHints > 0 && hands(pid).exists { cid => cid != CardId.NULL && rules.hintApplies(hint,cardMap(cid)) }
    }
  }

  def seenAction(ga: GiveAction): SeenAction = {
    ga match {
      case GiveDiscard(hid) => SeenDiscard(hid,hands(curPlayer)(hid))
      case GivePlay(hid) =>
        if(isPlayable(cardMap(hands(curPlayer)(hid))))
          SeenPlay(hid,hands(curPlayer)(hid))
        else
          SeenBomb(hid,hands(curPlayer)(hid))
      case GiveHint(pid,hint) =>
        val appliedTo = hands(pid).mapCards { cid => cid != CardId.NULL && rules.hintApplies(hint,cardMap(cid)) }
        SeenHint(pid,rules.seenHint(hint),appliedTo)
    }
  }

  def isPlayable(card: Card): Boolean = {
    nextPlayable(card.color.id) == card.number
  }

  def doAction(ga: GiveAction): Unit = {
    var shouldDraw = false
    ga match {
      case GiveDiscard(hid) =>
        val cid = hands(curPlayer).remove(hid)
        shouldDraw = true
        numHints += 1
        numDiscarded += 1
        discarded = cid :: discarded
      case GivePlay(hid) =>
        val cid = hands(curPlayer).remove(hid)
        shouldDraw = true

        val card = cardMap(cid)
        if(isPlayable(card)) {
          numPlayed += 1
          nextPlayable(card.color.id) += 1
          played = cid :: played
          if(rules.extraHintFromPlaying(card.number))
            numHints = Math.min(numHints+1,rules.maxHints)
        }
        else {
          numDiscarded += 1
          numBombs += 1
          discarded = cid :: discarded
        }
      case GiveHint(_,_) =>
        numHints -= 1
    }

    if(shouldDraw) {
      deck match {
        case Nil => ()
        case cid :: rest =>
          deck = rest
          hands(curPlayer).add(cid)
          if(rest.isEmpty && finalTurnsLeft < 0)
            finalTurnsLeft = rules.numPlayers
      }
    }

    curPlayer = (curPlayer + 1) % rules.numPlayers

    if(finalTurnsLeft > 0)
      finalTurnsLeft -= 1
    turnNumber += 1
  }

  def replaceCardMap(newCardMap: CardMap): Unit = {
    cardMap = newCardMap
  }

  def hideDeck(): Unit = {
    deck.foreach { cid => cardMap(cid) = Card.NULL }
  }

  def hideFor(pid: PlayerId): Unit = {
    deck.foreach { cid => cardMap(cid) = Card.NULL }
    hands(pid).foreach { cid => if(cid != CardId.NULL) cardMap(cid) = Card.NULL }
  }

  def hiddenFor(pid: PlayerId): Game = {
    val copy = Game(this)
    copy.hideFor(pid)
    copy
  }

  def drawInitialCards(): Unit = {
    for(pid <- 0 to (rules.numPlayers - 1)) {
      for(i <- 0 to (rules.handSize - 1)) {
        deck match {
          case Nil => throw new Exception("Not enough cards in deck to draw initial cards")
          case cid :: rest =>
            deck = rest
            hands(pid).add(cid)
        }
      }
    }
    if(deck.isEmpty && finalTurnsLeft < 0)
      finalTurnsLeft = rules.numPlayers
  }

  def isDone(): Boolean = {
    numBombs > rules.maxBombs ||
    numDiscarded > rules.maxDiscards ||
    numPlayed >= rules.maxScore ||
    finalTurnsLeft == 0
  }

  def isWon(): Boolean = {
    numPlayed == rules.maxScore
  }

  def toString(useAnsiColors: Boolean): String = {
    val handsString = (0 to (rules.numPlayers-1)).map { pid =>
      val toPlayString = if(pid == curPlayer) "*" else " "
      toPlayString + "P" + pid + ": " + hands(pid).toString(cardMap,useAnsiColors)
    }.mkString("|")

    val playedString = rules.colors().flatMap { color =>
      val next = nextPlayable(color.id)
      if(next <= 1)
        None
      else
        Some(Card(color,next-1).toString(useAnsiColors))
    }.mkString("")

    val dangerString = discarded.map { cid => cardMap(cid) }.sorted.flatMap { card =>
      if(card.number >= nextPlayable(card.color.id))
        Some(card.toString(useAnsiColors))
      else
        None
    }.mkString("")

    "T%3d HL %d NB %d ND %2d Played %s %s danger %s".format(
      turnNumber,
      numHints,
      numBombs,
      numDiscarded,
      playedString,
      handsString,
      dangerString
    )
  }

  def seenActionToString(sa: SeenAction, useAnsiColors: Boolean): String = {
    sa match {
      case SeenDiscard(hid,cid) =>
        "Discard #%d %s".format(hid+1,cardMap(cid).toString(useAnsiColors))
      case SeenPlay(hid,cid) =>
        "Play #%d %s".format(hid+1,cardMap(cid).toString(useAnsiColors))
      case SeenBomb(hid,cid) =>
        "Bomb #%d %s".format(hid+1,cardMap(cid).toString(useAnsiColors))
      case SeenHint(pid,hint,appliedTo) =>
        val hintString = hint match {
          case HintColor(color) =>
            if(useAnsiColors)
              color.toAnsiColorCode() + color.toString() + Color.ansiResetColor
            else
              color.toString()
          case HintNumber(number) =>
            number.toString()
          case HintSameColor =>
            "color"
          case HintSameNumber =>
            "number"
          case HintSame =>
            ""
        }

        val appliedString = appliedTo.zipWithIndex.flatMap { case (b,hid) =>
          if(b) Some((hid+1).toString)
          else None
        }.mkString("")

        "Hint P%d %s #%s".format(pid,hintString,appliedString)
    }
  }

  //For debug purposes
  def giveActionToString(ga: GiveAction): String = {
    ga match {
      case GiveDiscard(hid) => "Discard #" + (hid+1)
      case GivePlay(hid) => "Play #" + (hid+1)
      case GiveHint(pid,hint) =>
        val hintString = hint match {
          case HintColor(color) => color.toString()
          case HintNumber(number) => number.toString()
        }
        "Hint " + hintString
    }
  }
}
