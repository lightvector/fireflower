package fireflower

import RichImplicits._

object Game {
  def apply(rules: Rules, seed: Long): Game = {
    val cardMap = CardMap(rules,Rand(seed))
    new Game(
      rules = rules,
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
      nextPlayable = Array.fill(rules.maxColorId-1)(1),
      revHistory = List()
    )
  }

  def apply(that: Game): Game = {
    new Game(
      rules = that.rules,
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
  var numHints: Int,
  var numBombs: Int,
  var numPlayed: Int,
  var numDiscarded: Int,
  val cardMap: CardMap,
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
        numHints > 0 && hands(pid).exists { cid => rules.hintApplies(hint,cardMap(cid)) }
    }
  }

  def seenAction(ga: GiveAction): SeenAction = {
    ga match {
      case GiveDiscard(hid) => SeenDiscard(hid,hands(curPlayer)(hid))
      case GivePlay(hid) => SeenPlay(hid,hands(curPlayer)(hid))
      case GiveHint(pid,hint) =>
        val appliedTo = hands(pid).mapCards { cid => rules.hintApplies(hint,cardMap(cid)) }
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
  }



}
