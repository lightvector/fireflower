package fireflower

import RichImplicits._

object Game {
  def apply(rules: Rules, seed: Long): Game = {
    val seenMap = SeenMap(rules,Rand(seed))
    val numCardRemaining = Array.fill(Card.maxArrayIdx)(0)
    val nextPlayable = Array.fill(Color.LIMIT)(-1)
    seenMap.cards.foreach { card =>
      numCardRemaining(card.arrayIdx) += 1
      nextPlayable(card.color.id) = 0
    }
    new Game(
      rules = rules,
      turnNumber = 0,
      numHints = rules.initialHints,
      numBombs = 0,
      numPlayed = 0,
      numDiscarded = 0,
      numUnknownHintsGiven = 0,
      seenMap = seenMap,
      played = List(),
      discarded = List(),
      deck = (0 to (rules.deckSize-1)).toList,
      curPlayer = 0,
      finalTurnsLeft = -1,
      hands = Array.fill(rules.numPlayers)(Hand(rules.handSize)),
      nextPlayable = nextPlayable,
      numCardRemaining = numCardRemaining,
      revHistory = List(),
      debugPath = None
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
      numUnknownHintsGiven = that.numUnknownHintsGiven,
      seenMap = SeenMap(that.seenMap),
      played = that.played,
      discarded = that.discarded,
      deck = that.deck,
      curPlayer = that.curPlayer,
      finalTurnsLeft = that.finalTurnsLeft,
      hands = that.hands.map { hand => Hand(hand) },
      nextPlayable = that.nextPlayable.clone(),
      numCardRemaining = that.numCardRemaining.clone(),
      revHistory = that.revHistory,
      debugPath = that.debugPath
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
  var numUnknownHintsGiven: Int, //Used by the HeuristicPlayer
  var seenMap: SeenMap,
  var played: List[CardId],
  var discarded: List[CardId],
  var deck: List[CardId],
  var curPlayer: PlayerId,
  var finalTurnsLeft: Int,
  val hands: Array[Hand],
  val nextPlayable: Array[Int], //Indexed by ColorId
  val numCardRemaining: Array[Int], //Number of this card remaining in deck or hand, indexed by card.arrayIdx
  val revHistory: List[SeenAction],
  var debugPath: Option[List[GiveAction]]
) {

  val possibleHintTypes = rules.possibleHintTypes()

  def isLegal(ga: GiveAction): Boolean = {
    ga match {
      case GiveDiscard(hid) =>
        numHints < rules.maxHints && hid >= 0 && hid < hands(curPlayer).numCards
      case GivePlay(hid) =>
        hid >= 0 && hid < hands(curPlayer).numCards
      case GiveHint(pid,hint) =>
        numHints > 0 &&
        pid != curPlayer &&
        hint != UnknownHint &&
        possibleHintTypes.exists { ht => hint == ht } &&
        hands(pid).exists { cid => rules.hintApplies(hint,seenMap(cid)) }
    }
  }

  def seenAction(ga: GiveAction): SeenAction = {
    ga match {
      case GiveDiscard(hid) => SeenDiscard(hid,hands(curPlayer)(hid))
      case GivePlay(hid) =>
        if(isPlayable(seenMap(hands(curPlayer)(hid))))
          SeenPlay(hid,hands(curPlayer)(hid))
        else
          SeenBomb(hid,hands(curPlayer)(hid))
      case GiveHint(pid,hint) =>
        val appliedTo = hands(pid).mapCards { cid => rules.hintApplies(hint,seenMap(cid)) }
        SeenHint(pid,rules.seenHint(hint),appliedTo)
    }
  }

  def isPlayable(card: Card): Boolean = {
    nextPlayable(card.color.id) == card.number
  }
  def isUseful(card: Card): Boolean = {
    nextPlayable(card.color.id) <= card.number
  }
  def isDangerous(card: Card): Boolean = {
    nextPlayable(card.color.id) <= card.number &&
    numCardRemaining(card.arrayIdx) <= 1
  }
  def isJunk(card: Card): Boolean = {
    nextPlayable(card.color.id) > card.number
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
        val card = seenMap(cid)
        val cardArrayIdx = card.arrayIdx
        if(numCardRemaining(cardArrayIdx) > 0)
          numCardRemaining(cardArrayIdx) -= 1
      case GivePlay(hid) =>
        val cid = hands(curPlayer).remove(hid)
        shouldDraw = true

        val card = seenMap(cid)
        if(isPlayable(card)) {
          numPlayed += 1
          nextPlayable(card.color.id) += 1
          played = cid :: played
          if(rules.extraHintFromPlayingMax && card.number == rules.maxNumber)
            numHints = Math.min(numHints+1,rules.maxHints)

          val cardArrayIdx = card.arrayIdx
          numCardRemaining(cardArrayIdx) = -1
        }
        else {
          numDiscarded += 1
          numBombs += 1
          discarded = cid :: discarded
          val cardArrayIdx = card.arrayIdx
          if(numCardRemaining(cardArrayIdx) > 0)
            numCardRemaining(cardArrayIdx) -= 1
        }
      case GiveHint(_,hint) =>
        numHints -= 1
        hint match {
          case UnknownHint => numUnknownHintsGiven += 1
          case _ => ()
        }
    }

    if(shouldDraw) {
      deck match {
        case Nil => ()
        case cid :: rest =>
          deck = rest
          hands(curPlayer).add(cid)
          if(rest.isEmpty && finalTurnsLeft < 0)
            finalTurnsLeft = rules.numPlayers + 1 //+1 because will get 1 subtracted from it below
      }
    }

    curPlayer = (curPlayer + 1) % rules.numPlayers

    if(finalTurnsLeft > 0)
      finalTurnsLeft -= 1
    turnNumber += 1

    debugPath = debugPath match {
      case None => None
      case Some(Nil) => None
      case Some(action::rest) =>
        if(action != ga) None
        else Some(rest)
    }
  }

  def replaceSeenMap(newSeenMap: SeenMap): Unit = {
    seenMap = newSeenMap
  }

  def hideDeck(): Unit = {
    deck.foreach { cid => seenMap(cid) = Card.NULL }
  }

  def hideFor(pid: PlayerId): Unit = {
    deck.foreach { cid => seenMap(cid) = Card.NULL }
    hands(pid).foreach { cid => seenMap(cid) = Card.NULL }
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
    finalTurnsLeft == 0 ||
    (rules.stopEarlyLoss && (
      numDiscarded > rules.maxDiscards ||
        numPlayed >= rules.maxScore ||
        discarded.exists { cid =>
          val card = seenMap(cid)
          card.number >= nextPlayable(card.color.id) && numCardRemaining(card.arrayIdx) == 0
        }
    ))
  }

  def isWon(): Boolean = {
    numPlayed == rules.maxScore
  }

  def toString(useAnsiColors: Boolean): String = {
    val handsString = (0 to (rules.numPlayers-1)).map { pid =>
      val toPlayString = if(pid == curPlayer) "*" else " "
      toPlayString + "P" + pid + ": " + hands(pid).toString(seenMap,useAnsiColors)
    }.mkString("|")

    val playedString = rules.colors().flatMap { color =>
      val next = nextPlayable(color.id)
      if(next <= 0)
        None
      else
        Some(Card(color,next-1).toString(useAnsiColors))
    }.mkString("")

    val dangerString = discarded.map { cid => seenMap(cid) }.sorted.flatMap { card =>
      if(card.number >= nextPlayable(card.color.id))
        Some(card.toString(useAnsiColors))
      else
        None
    }.mkString("")

    val endRoundString = {
      if(finalTurnsLeft >= 0)
        "final" + finalTurnsLeft
      else
        ""
    }

    "T%3d HL %d NB %d ND %2d Played %s %s danger %s %s".format(
      turnNumber,
      numHints,
      numBombs,
      numDiscarded,
      playedString,
      handsString,
      dangerString,
      endRoundString
    )
  }

  def seenActionToString(sa: SeenAction, useAnsiColors: Boolean): String = {
    sa match {
      case SeenDiscard(hid,cid) =>
        "Discard #%d %s".format(hid+1,seenMap(cid).toString(useAnsiColors))
      case SeenPlay(hid,cid) =>
        "Play #%d %s".format(hid+1,seenMap(cid).toString(useAnsiColors))
      case SeenBomb(hid,cid) =>
        "Bomb #%d %s".format(hid+1,seenMap(cid).toString(useAnsiColors))
      case SeenHint(pid,hint,appliedTo) =>
        val hintString = hint match {
          case HintColor(color) =>
            if(useAnsiColors)
              color.toAnsiColorCode() + color.toString() + Color.ansiResetColor
            else
              color.toString()
          case HintNumber(number) =>
            (number+1).toString()
          case HintSameColor =>
            "color"
          case HintSameNumber =>
            "number"
          case HintSame =>
            ""
          case UnknownHint =>
            "UnknownHint"
        }

        val appliedString = appliedTo.zipWithIndex.flatMap { case (b,hid) =>
          if(b) Some(seenMap(hands(pid)(hid)).toString(useAnsiColors))
          else None
        }.mkString("")

        "Hint P%d %s %s".format(pid,hintString,appliedString)
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
          case HintNumber(number) => (number+1).toString()
          case UnknownHint => "Unknown"
        }
        "Hint " + hintString
    }
  }
}
