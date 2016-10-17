package fireflower

//Beliefs that we track per-card that stays attached to card as it moves in a hand.
sealed trait Belief

//Many of the following come in pairs, one shared between all cards that were involved
//in the same event, and a second that contains the former as a field that actually extends
//Info and is the value that is tracked per-card.

//A hint was received - this info is meant to track the purely logical info learned.
case class Hinted(sh: SeenHint, hand: Array[CardId])
case class HintedInfo(hid: HandId, applied: Boolean, hinted: Hinted)

//We think the following cards are playable
//and should be played in this order. Possibly includes cards in other player's hands.
case class PlaySequence(cids: Array[CardId])
case class PlaySequenceBelief(seqIdx: Int, ps: PlaySequence) extends Belief

//We think that these cards are protected and should be held onto and not discarded
case class ProtectedSet(cids: Array[CardId])
case class ProtectedSetBelief(seqIdx: Int, ps: ProtectedSet) extends Belief

//We think these cards can be thrown away
case class JunkSet(cids: Array[CardId])
case class JunkSetBelief(seqIdx: Int, js: JunkSet) extends Belief

class HeuristicPlayer(val seed: Long, val myPid: Int, val rules: Rules) extends Player {
  val rand: Rand = Rand(Array(seed,myPid.toLong))
  val possibleHintTypes: Array[GiveHintType] = rules.possibleHintTypes()
  val maxHints: Int = rules.maxHints
  val uniqueCards: List[Card] = rules.cards().distinct.toList

  var seenMap: SeenMap = SeenMap.empty(rules)
  var seenMapCK: SeenMap = SeenMap.empty(rules)

  val infoMap: CardPropertyMap[HintedInfo] = CardPropertyMap(rules)
  val beliefMap: CardPropertyMap[Belief] = CardPropertyMap(rules)

  def updateSeenMap(game: Game): Unit = {
    seenMap = SeenMap(game.seenMap)
    seenMapCK = SeenMap(game.seenMap)
    (0 to (rules.numPlayers-1)).foreach { pid =>
      game.hands(pid).foreach { cid => seenMapCK(cid) = Card.NULL }
    }
  }

  def possibleCards(cid: CardId, ck: Boolean): List[Card] = {
    var sm = seenMap
    if(ck) sm = seenMapCK

    val seenCard = sm(cid)
    if(seenCard != Card.NULL)
      List(seenCard)
    else {
      val possibles: List[Card] = sm.uniqueUnseen()
      infoMap(cid).foldLeft(possibles) { case (possibles,info) =>
        possibles.filter { card => rules.isConsistent(info.hinted.sh.hint, info.applied, card) }
      }
    }
  }

  def provablyPlayable(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isPlayable(card) }
  }
  def provablyUseful(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isUseful(card) }
  }
  def provablyDangerous(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isDangerous(card) }
  }
  def provablyJunk(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isJunk(card) }
  }

  def isBelievedProtected(cid: CardId) = {
    beliefMap(cid) match {
      case Nil => false
      case belief :: rest =>
        belief match {
          case (_: ProtectedSetBelief) => true
          case (_: PlaySequenceBelief) => false
          case (_: JunkSetBelief) => false
        }
    }
  }
  def isBelievedPlayable(cid: CardId, now: Boolean) = {
    beliefMap(cid) match {
      case Nil => false
      case belief :: rest =>
        belief match {
          case (_: ProtectedSetBelief) => false
          case (b: PlaySequenceBelief) => if (now) b.seqIdx == 0 else true
          case (_: JunkSetBelief) => false
        }
    }
  }
  def isBelievedUseful(cid: CardId) = {
    beliefMap(cid) match {
      case Nil => false
      case belief :: rest =>
        belief match {
          case (_: ProtectedSetBelief) => true
          case (_: PlaySequenceBelief) => true
          case (_: JunkSetBelief) => false
        }
    }
  }
  def isBelievedJunk(cid: CardId) = {
    beliefMap(cid) match {
      case Nil => false
      case belief :: rest =>
        belief match {
          case (_: ProtectedSetBelief) => false
          case (_: PlaySequenceBelief) => false
          case (_: JunkSetBelief) => true
        }
    }
  }

  def believedCard(cid: CardId): Card = {
    //TODO
    return Card.NULL
  }

  def isBelievedDangerous(cid: CardId, game: Game) = {
    val card = believedCard(cid)
    card != Card.NULL && game.isDangerous(card)
  }


  type DiscardGoodness = Int
  val DISCARD_PROVABLE_JUNK: DiscardGoodness = 6
  val DISCARD_JUNK: DiscardGoodness = 5
  val DISCARD_REGULAR: DiscardGoodness = 4
  val DISCARD_USEFUL: DiscardGoodness = 3
  val DISCARD_PLAYABLE: DiscardGoodness = 2
  val DISCARD_MAYBE_GAMEOVER: DiscardGoodness = 1
  val DISCARD_GAMEOVER: DiscardGoodness = 0

  def mostLikelyDiscard(pid: PlayerId, game: Game): (HandId,DiscardGoodness) = {
    val revHand: Array[CardId] = game.hands(pid).cardArray().reverse
    val numCards = revHand.length
    val ck = pid == myPid
    val possibles: Array[List[Card]] = revHand.map { cid => possibleCards(cid,ck) }

    val (pos,dg): (Int,DiscardGoodness) = {
      val provableJunkDiscard = (0 to (numCards-1)).find { pos => provablyJunk(possibles(pos),game) }
      provableJunkDiscard match {
        case Some(pos) => (pos,DISCARD_PROVABLE_JUNK)
        case None =>
          val junkDiscard = (0 to (numCards-1)).find { pos => isBelievedJunk(revHand(pos)) && !provablyUseful(possibles(pos),game) }
          junkDiscard match {
            case Some(pos) => (pos,DISCARD_JUNK)
            case None =>
              val regularDiscard = (0 to (numCards-1)).find { pos => !isBelievedUseful(revHand(pos)) && !provablyUseful(possibles(pos),game) }
              regularDiscard match {
                case Some(pos) => (pos,DISCARD_REGULAR)
                case None =>
                  val usefulDiscard = (0 to (numCards-1)).find { pos =>
                    !isBelievedPlayable(revHand(pos),now=false) &&
                    !isBelievedDangerous(revHand(pos),game) &&
                    !provablyDangerous(possibles(pos),game) &&
                    !provablyPlayable(possibles(pos),game)
                  }
                  usefulDiscard match {
                    case Some(pos) => (pos,DISCARD_USEFUL)
                    case None =>
                      val playableDiscard = (0 to (numCards-1)).find { pos =>
                        !isBelievedDangerous(revHand(pos),game) &&
                        !provablyDangerous(possibles(pos),game)
                      }
                      playableDiscard match {
                        case Some(pos) => (pos,DISCARD_PLAYABLE)
                        case None =>
                          val maybeGameOverDiscard = (0 to (numCards-1)).find { pos =>
                            !provablyDangerous(possibles(pos),game)
                          }
                          maybeGameOverDiscard match {
                            case Some(pos) => (pos,DISCARD_MAYBE_GAMEOVER)
                            case None => (0,DISCARD_GAMEOVER)
                          }
                      }
                  }
              }
          }
      }
    }
    (numCards-1-pos,dg)
  }


  override def handleGameStart(game: Game): Unit = {
    updateSeenMap(game)
  }

  override def handleSeenAction(preGame: Game, sa: SeenAction, postGame: Game): Unit = {
    updateSeenMap(postGame)
  }

  override def getAction(game: Game): GiveAction = {
    updateSeenMap(game)
  }



}
