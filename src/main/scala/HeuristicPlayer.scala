/**
  * HeuristicPlayer.scala
  * Plays according to bunch of simple hardcoded conventions that dictate how cards are signaled
  * as playable or discardable or protectworthy by hints or other actions.
  *
  * However, conventions mostly do not dictate the actions taken. Instead, those are computed by
  * looping over all actions and running a pseudo-depth-2 search by predicting the opponent's likely
  * action or actions and then running an evaluation function, and choosing the action that leads
  * to the highest expected evaluation.
  */

package fireflower

import RichImplicits._

//Below are a bunch of types for different kinds of beliefs or knowledge. Each comes in a pair,
//with an "*Info" that is shared between all cards involved in that belief or piece of knowledge,
//and a second type that contains the former as a field that is tracked per-card.

//All the arrays in these information-related are read-only and should NOT be modified.

//The shared information between all cards connected in a belief
sealed trait BeliefInfo
//The value we track per-card that stays attached to card as it moves in a hand.
sealed trait Belief {
  override def toString(): String = {
    this match {
      case PlaySequence(seqIdx,info) =>
        "PlaySequence(seqIdx=" + seqIdx + ",cids=(" + info.cids.mkString(",") + "))"
      case ProtectedSet(seqIdx,info) =>
        "ProtectedSet(seqIdx=" + seqIdx + ",cids=(" + info.cids.mkString(",") + "))"
      case JunkSet(seqIdx,info) =>
        "JunkSet(seqIdx=" + seqIdx + ",cids=(" + info.cids.mkString(",") + "))"
    }
  }
}

//A hint was received - this info is meant to track the purely logical info learned.
case class HintedInfo(sh: SeenHint, hand: Array[CardId])
case class Hinted(hid: HandId, applied: Boolean, info: HintedInfo)

//We think the following cards are playable
//and should be played in this order. Possibly includes cards in other player's hands.
case class PlaySequenceInfo(cids: Array[CardId]) extends BeliefInfo
case class PlaySequence(seqIdx: Int, info: PlaySequenceInfo) extends Belief

//We think that these cards are protected and should be held onto and not discarded
case class ProtectedSetInfo(cids: Array[CardId]) extends BeliefInfo
case class ProtectedSet(seqIdx: Int, info: ProtectedSetInfo) extends Belief

//We think these cards can be thrown away
case class JunkSetInfo(cids: Array[CardId]) extends BeliefInfo
case class JunkSet(seqIdx: Int, info: JunkSetInfo) extends Belief

//Basic constructors and other static functions for the player
object HeuristicPlayer extends PlayerGen {

  //Construct a HeuristicPlayer for the given rule set
  def apply(rules: Rules, myPid: Int): HeuristicPlayer = {
    val numCardsInitial = Array.fill(Card.maxArrayIdx)(0)
    rules.cards().foreach { card =>
      numCardsInitial(card.arrayIdx) += 1
    }
    new HeuristicPlayer(
      myPid = myPid,
      rules = rules,
      possibleHintTypes = rules.possibleHintTypes(),
      maxHints = rules.maxHints,
      distinctCards = rules.cards().distinct.toList,
      numCardsInitial = numCardsInitial,
      colors = rules.colors(),
      seenMap = SeenMap.empty(rules),
      seenMapCK = SeenMap.empty(rules),
      hintedMap = CardPropertyMap(rules),
      beliefMap = CardPropertyMap(rules)
    )
  }

  //PlayerGen interface - Generate a set of players for a game.
  def genPlayers(rules: Rules, seed: Long): Array[Player] = {
    (0 to (rules.numPlayers-1)).map { myPid =>
      this(rules,myPid)
    }.toArray
  }
}

case class SavedState(
  val seenMap: SeenMap,
  val seenMapCK: SeenMap,
  val hintedMap: CardPropertyMap[Hinted],
  val beliefMap: CardPropertyMap[Belief]
)

class HeuristicPlayer private (
  //IMMUTABLE-------------------------------------------
  val myPid: Int,
  val rules: Rules,

  //Various utility values we compute once and cache
  val maxHints: Int,
  val possibleHintTypes: Array[GiveHintType],
  val distinctCards: List[Card],   //contains each distinct card type once
  val numCardsInitial: Array[Int], //indexed by card.arrayIdx, counts the quantity of that card in the whole deck
  val colors: Array[Color],        //an array of the colors in this game

  //STATE-----------------------------------------------

  //Tracks what cards are visible by us
  val seenMap: SeenMap,
  //Tracks what cards are visible as common knowledge
  val seenMapCK: SeenMap,

  //Logical information we've received via hints, tracked by card
  val hintedMap: CardPropertyMap[Hinted],
  //Beliefs we have about cards based on conventions.
  //In general, it's important that this map doesn't contain things that can be inferred only based on
  //private information because it's used to predict other players' actions.
  val beliefMap: CardPropertyMap[Belief]

) extends Player {

  def saveState(): SavedState = {
    SavedState(
      seenMap = SeenMap(seenMap),
      seenMapCK = SeenMap(seenMapCK),
      hintedMap = CardPropertyMap(hintedMap),
      beliefMap = CardPropertyMap(beliefMap)
    )
  }
  def restoreState(saved: SavedState): Unit = {
    saved.seenMap.copyTo(seenMap)
    saved.seenMapCK.copyTo(seenMapCK)
    saved.hintedMap.copyTo(hintedMap)
    saved.beliefMap.copyTo(beliefMap)
  }

  //Checks whether the current game state is one where we should be printing debug messages.
  def debugging(game: Game): Boolean = {
    game.debugPath match {
      case None => false
      case Some(_) => true
    }
  }

  //Update the seen maps based on a new incoming game state
  def updateSeenMap(game: Game): Unit = {
    game.seenMap.copyTo(seenMap)
    game.seenMap.copyTo(seenMapCK)
    (0 to (rules.numPlayers-1)).foreach { pid =>
      game.hands(pid).foreach { cid => seenMapCK(cid) = Card.NULL }
    }
  }

  //Add a belief via its shared info, computing the per-card values to store
  def addBelief(info: BeliefInfo): Unit = {
    info match {
      case (info:PlaySequenceInfo) =>
        for(i <- 0 to (info.cids.length-1))
          beliefMap.add(info.cids(i),PlaySequence(seqIdx=i,info=info))
      case (info:ProtectedSetInfo) =>
        for(i <- 0 to (info.cids.length-1))
          beliefMap.add(info.cids(i),ProtectedSet(seqIdx=i,info=info))
      case (info:JunkSetInfo) =>
        for(i <- 0 to (info.cids.length-1))
          beliefMap.add(info.cids(i),JunkSet(seqIdx=i,info=info))
    }
  }

  //Is every hint we've received consistent with cid being card?
  def allHintsConsistent(cid: CardId, card: Card): Boolean = {
    hintedMap(cid).forall { hinted => rules.isConsistent(hinted.info.sh.hint, hinted.applied, card) }
  }

  //TODO this function is called frequently!
  //Maybe we can memoize it - might be a decent speedup.

  //What cards could [cid] be as a strictly logical possiblity?
  //If ck is false, uses all information known.
  //If ck is true, uses only common knowledge information.
  def possibleCards(cid: CardId, ck: Boolean): List[Card] = {
    var sm = seenMap
    if(ck) sm = seenMapCK

    val seenCard = sm(cid)
    if(seenCard != Card.NULL) List(seenCard)
    else sm.filterDistinctUnseen { card => allHintsConsistent(cid,card) }
  }

  //If there is a unique possible value for this card, return it, else Card.NULL
  def uniquePossible(cid: CardId, ck: Boolean): Card = {
    var sm = seenMap
    if(ck) sm = seenMapCK

    val seenCard = sm(cid)
    if(seenCard != Card.NULL) seenCard
    else sm.filterUniqueDistinctUnseen { card => allHintsConsistent(cid,card) }
  }

  //Check if there is any possible value for this card. ALSO verifies consistency of cards we've seen.
  def hasPossible(cid: CardId): Boolean = {
    val seenCard = seenMap(cid)
    if(seenCard != Card.NULL) allHintsConsistent(cid,seenCard)
    else seenMap.existsUnseen { card => allHintsConsistent(cid,card) }
  }

  //Check if there is a unique possible color for this card conditioned on it being useful. If not, returns NullColor
  def uniquePossibleUsefulColor(cid: CardId, game: Game, ck: Boolean): Color = {
    var sm = seenMap
    if(ck) sm = seenMapCK

    val seenCard = sm(cid)
    if(seenCard != Card.NULL) {
      if(game.isUseful(seenCard)) seenCard.color
      else NullColor
    }
    else {
      val possibles = sm.filterDistinctUnseen { card => allHintsConsistent(cid,card) && game.isUseful(card) }
      possibles match {
        case Nil => NullColor
        case head :: tail =>
          if(tail.forall { card => card.color == head.color })
            head.color
          else
            NullColor
      }
    }
  }

  def provablyPlayable(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isPlayable(card) }
  }
  def provablyPlayableIfUseful(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isPlayable(card) || game.isJunk(card) }
  }
  def provablyNotPlayable(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => !game.isPlayable(card) }
  }
  def provablyUseful(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isUseful(card) }
  }
  def provablyNotDangerous(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => !game.isDangerous(card) }
  }
  def provablyDangerous(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isDangerous(card) }
  }
  def provablyJunk(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isJunk(card) }
  }

  //The most recent belief formed about this card, if any.
  def primeBelief(cid: CardId): Option[Belief] = {
    beliefMap(cid) match {
      case Nil => None
      case belief :: _ => Some(belief)
    }
  }

  def isBelievedProtected(cid: CardId): Boolean = {
    primeBelief(cid) match {
      case None => false
      case Some(_: ProtectedSet) => true
      case Some(_: PlaySequence) => false
      case Some(_: JunkSet) => false
    }
  }
  def isBelievedPlayable(cid: CardId, now: Boolean): Boolean = {
    primeBelief(cid) match {
      case None => false
      case Some(_: ProtectedSet) => false
      case Some(b: PlaySequence) => if (now) b.seqIdx == 0 else true
      case Some(_: JunkSet) => false
    }
  }
  def isBelievedUseful(cid: CardId): Boolean = {
    primeBelief(cid) match {
      case None => false
      case Some(_: ProtectedSet) => true
      case Some(_: PlaySequence) => true
      case Some(_: JunkSet) => false
    }
  }
  def isBelievedJunk(cid: CardId): Boolean = {
    primeBelief(cid) match {
      case None => false
      case Some(_: ProtectedSet) => false
      case Some(_: PlaySequence) => false
      case Some(_: JunkSet) => true
    }
  }

  def getPlaySequenceExn(cid: CardId): PlaySequence = {
    primeBelief(cid) match {
      case None => assertUnreachable()
      case Some(_: ProtectedSet) => assertUnreachable()
      case Some(_: JunkSet) => assertUnreachable()
      case Some(b: PlaySequence) => b
    }
  }

  //TODO can we use this? It didn't seem to help when using it in probablyCorrectlyBelievedPlayableSoon
  //If knowledge proves or if beliefs and conventions strongly suggest that this card should be a specific card, return
  //that card, otherwise return Card.NULL.
  def believedCard(cid: CardId, game: Game, ck: Boolean): Card = {
    var sm = seenMap
    if(ck) sm = seenMapCK
    val known = uniquePossible(cid, ck)
    if(known != Card.NULL) known
    else {
      primeBelief(cid) match {
        case None => Card.NULL
        case Some(_: ProtectedSet) => Card.NULL
        case Some(_: JunkSet) => Card.NULL
        case Some(b: PlaySequence) =>
          //Believed playable now
          if(b.seqIdx <= 0)
            sm.filterUniqueDistinctUnseen { card => allHintsConsistent(cid,card) && game.isPlayable(card) }
          //Believed playable later
          else {
            Card.NULL
            //TODO see whether this logic makes it better
            // val possibles = sm.filterDistinctUnseen { card => allHintsConsistent(cid,card) && game.isUseful(card) }
            // possibles match {
            //   case Nil => Card.NULL
            //   case head :: tail =>
            //     //If this card must be a certain color...
            //     if(tail.forall { card => card.color == head.color }) {
            //       val color = head.color
            //       //Check all earlier cards to count and see which this could be
            //       var simulatedNextPlayable = game.nextPlayable(color.id)
            //       def loop(seqIdx:Int): Card = {
            //         if(seqIdx >= b.seqIdx)
            //           possibles.find { card => card.number == simulatedNextPlayable }.getOrElse(Card.NULL)
            //         else {
            //           val card = sm.filterUniqueDistinctUnseen { card => allHintsConsistent(cid,card) && card.color == color && card.number == simulatedNextPlayable }
            //           if(card == Card.NULL)
            //             loop(seqIdx + 1)
            //           else {
            //             simulatedNextPlayable += 1
            //             loop(seqIdx + 1)
            //           }
            //         }
            //       }
            //       loop(0)
            //     }
            //     //If it doesn't have to be a certain color, we have no idea
            //     else Card.NULL
            // }
          }
      }
    }
  }

  //Check if to the best of our knowledge, based on what's actually visible and what we suspect, a given card will
  //be playable once it gets reached in play sequence. NOT COMMON KNOWLEDGE!
  def probablyCorrectlyBelievedPlayableSoon(cid: CardId, game: Game): Boolean = {
    primeBelief(cid) match {
      case None => false
      case Some(_: ProtectedSet) => false
      case Some(_: JunkSet) => false
      case Some(b: PlaySequence) =>
        if(b.seqIdx <= 0) {
          //TODO for some strange reason the bot wins more if this is changed to merely "uniquePossible(cid, ck=false)". Why???
          val card = believedCard(cid, game, ck=false)
          if(card == Card.NULL)
            false
          else
            game.isPlayable(card)
        }
        else {
          //TODO this requires all cards involved in the hint up to this point to be good
          //unless provably not
          //Maybe allow for bad ordering if correction hints could be given

          //Loop and see if the card becomes playable as we play in sequence
          val simulatedNextPlayable = game.nextPlayable.clone()
          def loopOk(seqIdx:Int, okIfStopHere:Boolean): Boolean = {
            if(seqIdx > b.seqIdx)
              okIfStopHere
            else {
              val cid = b.info.cids(seqIdx)
              //TODO for some strange reason the bot wins more if this is changed to merely "uniquePossible(cid, ck=false)". Why???
              val card = believedCard(cid, game, ck=false)

              //Don't have a guess as to what the card is - can't say that it's playable soon
              if(card == Card.NULL)
                false
              //It's playable next in sequence!
              else if(simulatedNextPlayable(card.color.id) == card.number) {
                simulatedNextPlayable(card.color.id) += 1
                loopOk(seqIdx+1,true) //Loop again and if we stop here, it was playable, so good.
              }
              //It's provably junk if the earlier cards play as expected
              else if(simulatedNextPlayable(card.color.id) > card.number) {
                loopOk(seqIdx+1,false) //Loop again and if we stop here, it wasn't playable, so not good.
              }
              //TODO maybe add a case for provably not being the next card to play...?
              else false
            }
          }
          loopOk(0,false)
        }
    }
  }

  type DiscardGoodness = Int
  val DISCARD_PROVABLE_JUNK: DiscardGoodness = 6
  val DISCARD_JUNK: DiscardGoodness = 5
  val DISCARD_REGULAR: DiscardGoodness = 4
  val DISCARD_USEFUL: DiscardGoodness = 3
  val DISCARD_PLAYABLE: DiscardGoodness = 2
  val DISCARD_MAYBE_GAMEOVER: DiscardGoodness = 1
  val DISCARD_GAMEOVER: DiscardGoodness = 0

  //Goodness and discard are by common knowledge if and only if ck is true
  def mostLikelyDiscard(pid: PlayerId, game: Game, ck: Boolean): (HandId,DiscardGoodness) = {
    val revHand: Array[CardId] = game.hands(pid).cardArray().reverse
    val numCards = revHand.length
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
              val regularDiscard = (0 to (numCards-1)).find { pos => !isBelievedUseful(revHand(pos)) }
              regularDiscard match {
                case Some(pos) => (pos,DISCARD_REGULAR)
                case None =>
                  val usefulDiscard = (0 to (numCards-1)).find { pos =>
                    !isBelievedPlayable(revHand(pos),now=false) &&
                    !provablyDangerous(possibles(pos),game) &&
                    !provablyPlayable(possibles(pos),game)
                  }
                  usefulDiscard match {
                    case Some(pos) => (pos,DISCARD_USEFUL)
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
    (numCards-1-pos,dg)
  }

  //Find all the hand positions we think the given player can play, by convention and belief and knowledge.
  //Now determines if the cards must be playable now rather than eventually.
  //ck determines if we only check using common knowledge or all observed info
  def expectedPlays(pid: PlayerId, game: Game, now: Boolean, ck: Boolean): List[HandId] = {
    val hand = game.hands(pid)
    (0 to (hand.numCards-1)).filter { hid =>
      val cid = hand(hid)
      val possibles = possibleCards(cid,ck)
      if(provablyNotPlayable(possibles,game))
        false
      else {
        //Provably playable
        provablyPlayable(possibles,game) ||
        //Or believed...
        {
          primeBelief(cid) match {
            case None => false
            case Some(_: JunkSet) => false
            //Protected and playable conditional on being useful
            case Some(_: ProtectedSet) =>
              provablyPlayableIfUseful(possibles,game)
            //Playable and right now
            case Some(b: PlaySequence) =>
              if(now) b.seqIdx == 0 else true
          }
        }

      }
    }.toList
  }

  def allBelievedPlays(game: Game): List[CardId] = {
    game.hands.flatMap { hand =>
      (0 to (hand.numCards-1)).flatMap { hid =>
        val cid = hand(hid)
        primeBelief(cid) match {
          case None => None
          case Some(_: ProtectedSet) => None
          case Some(_: JunkSet) => None
          case Some(_: PlaySequence) => Some(cid)
        }
      }
    }.toList
  }

  def firstPossiblyPlayableHid(game: Game, pid: PlayerId, ck: Boolean): Option[HandId] = {
    game.hands(pid).findIdx { cid => !provablyNotPlayable(possibleCards(cid,ck),game) }
  }

  // TODO A major item that seems to sink the bot a lot right now is bad handling of discards and plays and bombs
  // In particular, things like preferring to discard more after the opponent as (via not hinting you) signalled
  // that your hand is safer than you think. Or understanding that drawing new cards yourself means less clogging
  // of your partner later. Etc.

  //Handle a discard that we've seen, updating info and beliefmaps.
  //Assumes seenMap is already updated, but nothing else.
  def handleSeenDiscard(sd: SeenDiscard, postGame: Game): Unit = {
    val cid = sd.cid
    val card = seenMap(cid)

    //Check and update beliefs based on the discard if the discard turned out to not actually be junk
    if(card != Card.NULL && !postGame.isJunk(card)) {
      primeBelief(cid) match {
        //No prior belief
        case None => ()
        //Card was protected or was a believed play
        case Some(_: ProtectedSet) | Some(_: PlaySequence) =>
          //TODO also check that it was NOT the most likely discard?
          //TODO this massively hurts playing strength on 3 and 4 player. Why? A bug? For now we hack to 2-player only

          //If the card was playable right now, then it's a hint about a playable duplicate of that card.
          if(rules.numPlayers == 2 && postGame.isPlayable(card)) {
            //Find all players that have a copy of that card other than the discarder
            val discardPid = (postGame.curPlayer + (rules.numPlayers - 1)) % rules.numPlayers
            val hasCardAndNotDiscarder = (0 to (rules.numPlayers - 1)).map { pid =>
              if(pid == discardPid)
                false
              else
                postGame.hands(pid).exists { cid => seenMap(cid) == card }
            }.toArray

            val hasCardCount = hasCardAndNotDiscarder.count(x => x == true)

            //Which players does the hint signal
            val targetedPids: List[PlayerId] = {
              //If nobody has that card and it wasn't us giving the hint, it's us.
              if(hasCardCount == 0 && myPid != discardPid) List(myPid)
              //If nobody has that card and it was us giving that hint, it signals everyone else.
              else if(hasCardCount == 0 && myPid == discardPid) {
                (0 to rules.numPlayers-1).filter { pid => pid != myPid }.toList
              }
              //If exactly one other player than the discarder has the card, it signals them.
              else if(hasCardCount == 1) (0 to (rules.numPlayers - 1)).filter { pid => hasCardAndNotDiscarder(pid) }.toList
              //Else signals nobody
              else List()
            }

            def markPlayable(targetCid: CardId): Unit = {
              primeBelief(cid) match {
                //If old card was protected, then the new card is simply thought playable
                case Some(_: ProtectedSet) =>
                  addBelief(PlaySequenceInfo(cids = Array(cid)))
                //If old card was part of a sequence, then the new card is part of that sequence.
                case Some(PlaySequence(seqIdx,info)) =>
                  val cids = info.cids.clone
                  cids(seqIdx) = targetCid
                  addBelief(PlaySequenceInfo(cids))
                case _ =>
                  assert(false)
              }
            }

            targetedPids.foreach { pid =>
              //If there is a positively hinted card that by CK could be the card, mark the first such card as playable if it
              //is not already marked. Else mark the first card that could by CK be it if not already marked.
              val hand = postGame.hands(pid)

              hand.find { cid =>
                hintedMap(cid).exists { hinted => hinted.applied } &&
                possibleCards(cid,ck=true).contains(card)
              } match {
                case Some(cid) => markPlayable(cid)
                case None =>
                  hand.find { cid =>
                    possibleCards(cid,ck=true).contains(card)
                  } match {
                    case Some(cid) => markPlayable(cid)
                    case None => ()
                  }
              }
            }

          }

        //Card was believed junk
        case Some(b: JunkSet) =>
          //If the card was not actually junk, then immediately change everything in the believed junk set
          //to protected so we don't keep discarding them.
          addBelief(ProtectedSetInfo(cids = b.info.cids))
      }
    }
  }

  //Handle a play that we've seen, updating info and beliefmaps.
  //Assumes seenMap is already updated, but nothing else.
  def handleSeenPlay(sp: SeenPlay, postGame: Game): Unit = {
    val cid = sp.cid
    //Successful play
    primeBelief(cid) match {
      //No prior belief
      case None => ()
      //Card was protected - presumably the player somehow inferred it as playable
      case Some(_: ProtectedSet) => ()
      //Card was believed junk - presumably the player somehow inferred it as playable
      case Some(_: JunkSet) => ()
      //Card was a believed play. We actually don't need to do any updates here because
      //we later update all play sequences to remove newly provable junk cards.
      case Some(_: PlaySequence) => ()
    }
  }

  def handleSeenBomb(sb: SeenBomb, postGame: Game): Unit = {
    val cid = sb.cid
    primeBelief(cid) match {
      //No prior belief
      case None => ()
      //Card was protected - presumably the player somehow inferred it as playable but bombed??
      case Some(_: ProtectedSet) => ()
      //Card was believed junk - presumably the player somehow inferred it as playable or chose to play it anyways??
      case Some(_: JunkSet) => ()
      //Card was a believed play, but turned out to bomb.
      case Some(b: PlaySequence) =>
        //Immediately change everything in the sequence to protected if the card was useful
        //If it was junk, assume it was just an unfortunate collision
        val card = seenMap(cid)
        if(card != Card.NULL && postGame.isUseful(card)) {
          addBelief(ProtectedSetInfo(cids = b.info.cids))
        }
    }
  }

  //TODO some big items not yet implemented!
  // - hint to cause someone to bomb also indicates protection
  // - finesses/crossovers (for 3p and higher)
  // - protected two green cards with "G", green 2 out. Then hint the first one as 4 - should play the second then the first.

  //Handle a hint that we've seen, updating info and beliefmaps.
  //Assumes seenMap is already updated, but nothing else.
  def handleSeenHint(sh: SeenHint, postGame: Game): Unit = {
    val pid = sh.pid
    val hand = postGame.hands(pid)
    val hintCids = (0 to (hand.numCards-1)).flatMap { hid =>
      if(sh.appliedTo(hid)) Some(hand(hid))
      else None
    }.toArray

    //Prior to updating the hintedMap of what we and everyone knows about cards, figure out common
    //knowledge about what cards could have been what prior to this action.
    //handPrePossiblesCKByCid: For all cids in a hand, the possibles for those cids
    val handPrePossiblesCKByCid: Array[List[Card]] = Array.fill(rules.deckSize)(List())
    val prePossiblesCKByHand: Array[Array[List[Card]]] = postGame.hands.map { hand =>
      hand.cardArray().map { cid =>
        val possibles = possibleCards(cid,ck=true)
        handPrePossiblesCKByCid(cid) = possibles
        possibles
      }
    }

    //See what cards would have been be possible for the player to play by common knowledge
    val preExpectedPlaysNow: List[HandId] = expectedPlays(pid,postGame,now=true,ck=true)
    val preAllBelievedPlays: List[CardId] = allBelievedPlays(postGame) //TODO should this include provable plays?

    //See what card that player would have been likely to discard
    val (preMLD,preMLDGoodness): (HandId, DiscardGoodness) = mostLikelyDiscard(pid,postGame,ck=true)

    //Now update hintedMap with the logical information of the hint
    val hintedInfo = HintedInfo(sh, hand.cardArray())
    for (hid <- 0 to (hand.numCards-1)) {
      val hinted = Hinted(hid,sh.appliedTo(hid),hintedInfo)
      hintedMap.add(hand(hid),hinted)
    }

    //Check if it's a hint where the manner of the hint strongly indicates that it's a play hint
    //even if it would otherwise touch the most likely discard
    val isPlayEvenIfAffectingMLD: Boolean = {
      {
        //Hint affects at least one card that was not a play before and that could be playable now.
        hintCids.exists { cid =>
          val possibles = possibleCards(cid,ck=true)
          !provablyNotPlayable(possibles,postGame) //could be playable now
          !preExpectedPlaysNow.exists { hid => cid == hand(hid) } //not possible play before
        }
      } && {
        //All cards in hint are either provably junk, possibly playable, or completely known
        hintCids.forall { cid =>
          val possibles = possibleCards(cid,ck=true)
          !provablyNotPlayable(possibles,postGame) ||
          provablyJunk(possibles,postGame) ||
          possibles.length == 1
        }
      } && {
        sh.hint match {
          case HintNumber(num) =>
            //The number of cards possibly playable is >= the number of cards of this number that are useful.
            //OR all color piles are >= that number
            val numPossiblyPlayable = hintCids.count { cid =>
              val possibles = possibleCards(cid,ck=true)
              !provablyNotPlayable(possibles,postGame)
            }
            numPossiblyPlayable > colors.count { color => postGame.nextPlayable(color.id) <= num } ||
            colors.forall { color => postGame.nextPlayable(color.id) >= num }
          case HintColor(color) =>
            //Affects the first card and cards other than the first are already protected.
            sh.appliedTo(0) && (1 to (hand.length - 1)).forall { hid => isBelievedProtected(hand(hid)) }
          case _ => false
        }
      }
    }

    //Scan through all cids provided.
    //If some cids are provably of the same colors as other cards believed playable already, then
    //chain those cids on to the appropriate play sequence for those other cards, and filter them out of the array.
    def chainAndFilterFuturePlays(cids: Array[CardId]): Array[CardId] = {
      cids.filter { cid =>
        val color = uniquePossibleUsefulColor(cid, postGame, ck=true)
        var keep = true
        if(color != NullColor) {
          val earlierPlayCid = preAllBelievedPlays.find { playCid => playCid != cid && color == uniquePossibleUsefulColor(playCid, postGame, ck=true) }
          earlierPlayCid match {
            case None => ()
            case Some(earlierPlayCid) =>
              val info = getPlaySequenceExn(earlierPlayCid).info
              addBelief(PlaySequenceInfo(cids = info.cids :+ cid))
              keep = false
          }
        }
        keep
      }
    }

    //If this hint is an unknown hint, it does nothing
    if(sh.hint == UnknownHint)
    {}
    //If the hint targets the most likely discard
    //AND (there are no cards that the player would have played OR the hint touches a card we would have played)
    //AND (it's not a number hint where common knowledge says at least one MUST be playable)
    //AND (it's possible that the mld is a dangerous card after this hint)
    //then it's a protection hint.
    else if(
      sh.appliedTo(preMLD) &&
        (preExpectedPlaysNow.isEmpty || preExpectedPlaysNow.exists { hid => sh.appliedTo(hid) }) &&
        !isPlayEvenIfAffectingMLD &&
        !provablyNotDangerous(possibleCards(hand(preMLD),ck=true),postGame)
    ) {
      addBelief(ProtectedSetInfo(cids = hintCids))
    }
    //TODO this needs to be more sophisticated as well
    //Otherwise if at least one card hinted could be playable after the hint, then it's a play hint
    else if(hintCids.exists { cid => !provablyNotPlayable(possibleCards(cid,ck=true),postGame) }) {
      //Cards that are provably playable come first in the ordering
      val (hintCidsProvable, hintCidsNotProvable): (Array[CardId],Array[CardId]) =
        hintCids.partition { cid => provablyPlayable(possibleCards(cid,ck=true),postGame) }

      //Split out any cards that should belong to other play sequences
      val hintCidsNotProvable2 = chainAndFilterFuturePlays(hintCidsNotProvable)
      addBelief(PlaySequenceInfo(cids = hintCidsProvable ++ hintCidsNotProvable2))
    }
    //Otherwise if all cards in the hint are provably unplayable and not provably junk
    else if(hintCids.forall { cid =>
      val possibles = possibleCards(cid,ck=true)
      provablyNotPlayable(possibles,postGame) && !provablyJunk(possibles,postGame)
    }) {
      //Split out any cards that should belong to other play sequences
      val leftoverCids = chainAndFilterFuturePlays(hintCids)
      //Anything remaining treat as protected
      addBelief(ProtectedSetInfo(cids = leftoverCids))
    }
    //Otherwise if all cards in the hint are provably junk, then it's a protection hint
    //to all older cards that are not provably junk older than the oldest in the hint
    else if(hintCids.forall { cid => provablyJunk(possibleCards(cid,ck=true),postGame) }) {
      var oldestHintHid = 0
      for(hid <- 0 to (sh.appliedTo.length-1)) {
        if(sh.appliedTo(hid))
          oldestHintHid = hid
      }
      val protectedCids = ((oldestHintHid+1) to (sh.appliedTo.length-1)).map { hid => postGame.hands(pid)(hid) }
      addBelief(ProtectedSetInfo(cids = protectedCids.toArray))
    }
  }

  //Simplify and prune beliefs based on actual common-knowledge observations that may contradict them.
  def simplifyBeliefs(postGame: Game) = {
    //Array to avoid visiting each cid more than once
    val visited = Array.fill(rules.deckSize)(false)
    postGame.hands.foreach { hand =>
      hand.foreach { cid =>
        if(!visited(cid)) {
          primeBelief(cid) match {
            case None => ()
            //Filter protected sets down to only cards that could be dangerous
            case Some(b: ProtectedSet) =>
              b.info.cids.foreach { cid => visited(cid) = true }
              val (remainingCids,filteredCids) = b.info.cids.partition { cid => !provablyJunk(possibleCards(cid,ck=true),postGame) }
              val (newCids,playCids) = remainingCids.partition { cid => !provablyPlayableIfUseful(possibleCards(cid,ck=true),postGame) }
              if(filteredCids.length > 0) addBelief(JunkSetInfo(cids = filteredCids))
              if(playCids.length > 0) addBelief(PlaySequenceInfo(cids = playCids))
              if(newCids.length < b.info.cids.length) addBelief(ProtectedSetInfo(cids = newCids))

            //Filter junk sets down to only cards that could be safe
            case Some(b: JunkSet) =>
              b.info.cids.foreach { cid => visited(cid) = true }
              val (newCids,filteredCids) = b.info.cids.partition { cid => !provablyDangerous(possibleCards(cid,ck=true),postGame) }
              if(filteredCids.length > 0) {
                addBelief(JunkSetInfo(cids = newCids))
                addBelief(ProtectedSetInfo(cids = filteredCids))
              }

            //Filter play sequences down to only card ids that could be playable in that sequence given the cards before
            //Also remove cards from the play sequence that were superseeded by another belief
            case Some(b: PlaySequence) =>
              b.info.cids.foreach { cid => visited(cid) = true }
              var count = 0
              var expectedPlaysUpToNow: List[Card] = List()
              def possiblyPlayable(card: Card): Boolean = {
                postGame.isPlayable(card) ||
                expectedPlaysUpToNow.exists { c => c.color == card.color && c.number == card.number-1 }
              }
              def partOfThisSequence(cid: CardId): Boolean = {
                primeBelief(cid) match {
                  case Some(other: PlaySequence) => other.info eq b.info
                  case _ => false
                }
              }
              val remainingCids = b.info.cids.filter { cid => partOfThisSequence(cid) }
              val (newCids,filteredCids) = remainingCids.partition { cid =>
                count += 1
                if(count == b.info.cids.length)
                  possibleCards(cid,ck=true).exists { card => possiblyPlayable(card) }
                else {
                  val possiblePlays = possibleCards(cid,ck=true).filter { card => possiblyPlayable(card) }
                  if(possiblePlays.isEmpty)
                    false
                  else {
                    expectedPlaysUpToNow = possiblePlays ++ expectedPlaysUpToNow
                    true
                  }
                }
              }
              if(filteredCids.length > 0) {
                val (protectCids,junkCids) = filteredCids.partition { cid => !provablyJunk(possibleCards(cid,ck=true),postGame) }
                addBelief(PlaySequenceInfo(cids = newCids))
                addBelief(ProtectedSetInfo(cids = protectCids))
                addBelief(JunkSetInfo(cids = junkCids))
              }
          }
        }
      }
    }
  }

  //Check if the current state appears to be consistent.
  //Not exhaustive, but should catch most inconsistencies that might occur in practice.
  //(i.e. discarding things assuming them to be X and then finding out later that X must
  //still be in your hand due to more complex inferences that you didn't work out then)
  def checkIsConsistent(postGame: Game): Boolean = {
    postGame.hands.forall { hand =>
      hand.forall { cid => hasPossible(cid) }
    }
  }

  def softPlus(x: Double, width: Double) = {
    if(x/width >= 40.0) //To avoid floating point overflow
      40.0
    else
      Math.log(1.0 + Math.exp(x/width)) * width
  }

  //Maps from expected score space ("raw eval") -> goodness space ("eval")
  //This is a bit of a hack, because otherwise the horizon effect makes the bot highly reluctant to discard
  //due to fears of discarding the exact same card as partner is about to discard. By exping the values, we make
  //the averaging of that scenario have less effect.
  def transformEval(rawEval: Double) = {
    Math.exp(rawEval / 2.5)
  }
  def untransformEval(eval: Double) = {
    Math.log(eval) * 2.5
  }
  def evalToString(eval: Double) = {
    "%.1f (%.3f)".format(eval,untransformEval(eval))
  }

  //If we're not stopping on early losses, drop the raw eval by this many points for each point of score
  //we provably will miss a win by.
  val scoreDropPerLostPoint = 3.0

  def staticEvalGame(game: Game): Double = {
    if(game.isDone()) {
      if(rules.stopEarlyLoss)
        transformEval(game.numPlayed.toDouble)
      else
        transformEval(game.numPlayed.toDouble - scoreDropPerLostPoint * (rules.maxScore - game.numPlayed))
    }
    else {
      //PRELIMARIES-----------------------------------------------------------------------------------------
      //Compute some basic bounds and values used in the eval

      val turnsWithPossiblePlayLeft = {
        //On the last round
        if(game.finalTurnsLeft >= 0) {
          //Count remaining players who have a turn
          (0 to game.finalTurnsLeft-1).count { pidOffset =>
            val pid = (game.curPlayer + pidOffset) % rules.numPlayers
            //Whose hand has at least one possibly playable card.
            game.hands(pid).exists { cid => !provablyNotPlayable(possibleCards(cid,ck=false),game) }
          }
        }
        else {
          //Simply the number of possible playing turns left in the game
          game.deck.length + rules.numPlayers
        }
      }
      val maxPlaysLeft = {
        if(rules.stopEarlyLoss)
          rules.maxScore - game.numPlayed
        else {
          //Count up cards that are still useful taking into account dead piles.
          var usefulCardCount = 0
          colors.foreach { color =>
            var number = game.nextPlayable(color.id)
            while(game.numCardRemaining(Card.arrayIdx(color,number)) > 0 && number <= rules.maxNumber) {
              usefulCardCount += 1
              number += 1
            }
          }
          Math.min(usefulCardCount,turnsWithPossiblePlayLeft)
        }
      }
      //The amount by which we will provably miss the max score by
      val lossGap = rules.maxScore - game.numPlayed - maxPlaysLeft


      //NET HINTS-----------------------------------------------------------------------------------------
      //The most important term in the eval function - having more hints left in the game
      //(including in the future) is better.

      val numHints = game.numHints

      // TODO this helps on 3 and 4 player but hurts on 2-player!?
      // val numHintsAdjusted =
      //   if(numHints >= rules.maxHints) numHints - 0.70
      //   else if(numHints == rules.maxHints-1) numHints - 0.25
      //   else if(numHints == rules.maxHints-2) numHints - 0.05
      //   else numHints - 0.00

      val numDiscardsLeft = rules.maxDiscards - game.numDiscarded
      val numUnknownHintsGiven = game.numUnknownHintsGiven
      val numPotentialHints = {
        numDiscardsLeft +
        numHints +
        //Assume that unknown hints gain some value, even if we don't know what would be hinted
        numUnknownHintsGiven * 0.1 +
        {
          if(rules.extraHintFromPlayingMax)
            colors.count { color => game.nextPlayable(color.id) <= rules.maxNumber }
          else
            0
        }
      }

      //Adjustment - penalize for "bad" beliefs that need more hints to fix
      val fixupHintsRequired =
        game.hands.foldLeft(0.0) { case (acc,hand) =>
          hand.foldLeft(acc) { case (acc,cid) =>
            val card = game.seenMap(cid)
            val value = {
              if(card == Card.NULL)
                0.0
              else {
                val possibles = possibleCards(cid,ck=true)
                if(!provablyNotPlayable(possibles,game) &&
                  isBelievedPlayable(cid,now=true) &&
                  !game.isPlayable(card) &&
                  !game.isDangerous(card) //This because danger we often have to hint anyways, so no cost to have to fixup
                )
                  0.3
                else
                  0.0
              }
            }
            acc + value
          }
        }

      //Adjustment - bonus for "good" knowledge we already know that saves hints
      val goodKnowledge =
        game.hands.foldLeft(0.0) { case (acc,hand) =>
          hand.foldLeft(acc) { case (acc,cid) =>
            //TODO here and other places we use seenmap, consider using uniquePossible
            val card = game.seenMap(cid)
            val value = {
              if(probablyCorrectlyBelievedPlayableSoon(cid,game))
                0.55
              //TODO also add to the "isBelievedProtected(cid)" condition a check for whether it is
              //provably (ck=true) dangerous, or perhaps just whether the card is known exactly
              else if(isBelievedProtected(cid) && (card != Card.NULL && game.isDangerous(card)))
                0.2
              //TODO try this
              else if(isBelievedProtected(cid) && (card != Card.NULL && game.isPlayable(card)))
                0.1
              //TODO try stuff like this
              //else if(isBelievedJunk(cid) && (card == Card.NULL || game.isJunk(card)))
              //  0.1
              else
                0.0
            }
            //TODO possibly add a bonus for knowing color vs number?

            //TODO this should probably be a bit higher
            //Add a bonus for knowing the card exactly
            val exactBonus = if(uniquePossible(cid,ck=true) != Card.NULL) 0.01 else 0.00
            acc + value + exactBonus
          }
        }

      //All of the hint-related factors combined, and adjusted.
      val netFreeHints =
        numPotentialHints * 0.9 + goodKnowledge - (fixupHintsRequired + maxPlaysLeft) - 4
      //How much of the remaining score are we not getting due to lack of hints
      val hintScoreFactor = {
        val hintScoreFactorRaw = (maxPlaysLeft.toDouble + 3.0 - softPlus(-netFreeHints,2.5)) / (maxPlaysLeft + 3.0)
        //Avoid it going negative
        softPlus(hintScoreFactorRaw,0.1)
      }

      //LIMITED TIME/TURNS -----------------------------------------------------------------------------------------
      //Compute eval factors relating to having a limited amount of time or discards in the game.

      //TODO this has not been tested or tuned much
      //
      //How much of the remaining score are we not getting due to lack of turns
      val turnsLeftFactor = Math.min(maxPlaysLeft.toDouble, 0.8 * turnsWithPossiblePlayLeft) / maxPlaysLeft.toDouble

      //TODO currently not good, test again later
      // val discardLimitFactor = {
      //   if(rules.numPlayers >= 4) {
      //     if(game.numDiscarded > rules.maxDiscards) 0.500
      //     else if(game.numDiscarded == rules.maxDiscards) 0.800
      //     else if(game.numDiscarded == rules.maxDiscards-1) 0.920
      //     else if(game.numDiscarded == rules.maxDiscards-2) 0.980
      //     else if(game.numDiscarded == rules.maxDiscards-3) 0.995
      //     else 1.000
      //   }
      //   else if(rules.numPlayers >= 3) {
      //     if(game.numDiscarded > rules.maxDiscards) 0.500
      //     else if(game.numDiscarded == rules.maxDiscards) 0.880
      //     else if(game.numDiscarded == rules.maxDiscards-1) 0.960
      //     else if(game.numDiscarded == rules.maxDiscards-2) 0.990
      //     else if(game.numDiscarded == rules.maxDiscards-3) 0.998
      //     else 1.000
      //   }
      //   else {
      //     if(game.numDiscarded > rules.maxDiscards) 0.500
      //     else if(game.numDiscarded == rules.maxDiscards) 0.920
      //     else if(game.numDiscarded == rules.maxDiscards-1) 0.980
      //     else if(game.numDiscarded == rules.maxDiscards-2) 0.995
      //     else 1.000
      //   }
      // }

      //DANGER AND CLOGGING -----------------------------------------------------------------------------------------
      //Compute eval factors relating to having clogged hands or having discarded useful cards

      //TODO consider making this more principled - score based on the distribution of the remaining deck
      //and not merely dangerousness?

      //How much of the remaining score are we not getting due to danger stuff
      val dangerCount = distinctCards.foldLeft(0) { case (acc,card) =>
        if(card.number >= game.nextPlayable(card.color.id) &&
          game.isDangerous(card) &&
          seenMap.numUnseenByCard(card.arrayIdx) == 1)
          acc + (rules.maxNumber - card.number)
        else if(card.number >= game.nextPlayable(card.color.id) &&
          numCardsInitial(card.arrayIdx) > 2 &&
          game.numCardRemaining(card.arrayIdx) == 2 &&
          seenMap.numUnseenByCard(card.arrayIdx) == 2)
          acc + (rules.maxNumber - card.number) / 2
        else
          acc
      }
      val dangerFactor = Math.max(0.0, 1.0 - (dangerCount / 200.0))

      //TODO try adding a new term that adds a bonus for having at least a few playable cards in hand
      //so as to encourage saving them from discarding?

      //TODO clogginess should weigh less for protected cards that are playable after another protected card
      //and or similar, and they haven't been hinted yet.
      //TODO clogginess should be less for cards that are one-away from being playable
      //or for cards that literally are playable.
      val handClogFactor = game.hands.foldLeft(1.0) { case (acc,hand) =>
        val numClogs = hand.count { cid =>
          val card = seenMap(cid)
          //We can't see the card - either in our hand or we're a simulation for that player
          //This means it's safe to use ck=false, since we know no more than that player does, so whatever we
          //prove can be proven by them too.
          if(card == Card.NULL) {
            val possibles = possibleCards(cid,ck=false)
            if(provablyPlayable(possibles,game))
              false
            else if(isBelievedPlayable(cid,now=false))
              false
            else if(provablyJunk(possibles,game))
              false
            else if(provablyDangerous(possibles,game))
              true
            else
              isBelievedUseful(cid) && !isBelievedPlayable(cid,now=false)
          }
          //We can actually see the card
          else {
            //TODO playable cards can sometimes clog a hand if they're hard to hint out and/or the
            //player's current belief about them is wrong. Maybe experiment with this.
            if(game.isPlayable(card))
              false
            else if(probablyCorrectlyBelievedPlayableSoon(cid,game))
              false
            else if(game.isDangerous(card))
              true
            //TODO should we count believed-playable junk cards as clogging?
            else
              isBelievedUseful(cid)
          }
        }
        val value = {
          if(numClogs >= rules.handSize) 0.7
          else if(numClogs >= rules.handSize-1) 0.9
          else if(numClogs >= rules.handSize-2) 0.97
          else if(numClogs >= rules.handSize-3) 0.995
          else 1.0
        }
        acc * value
      }

      //BOMBS -----------------------------------------------------------------------------------------

      val bombsLeft = rules.maxBombs - game.numBombs + 1
      val bombsFactor = {
        if(bombsLeft >= 3) 1.0
        else if(bombsLeft == 2) 0.98
        else if(bombsLeft == 1) 0.93
        else 0.0
      }

      //PUT IT ALL TOGETHER -----------------------------------------------------------------------------------------

      val totalFactor = {
        dangerFactor *
        turnsLeftFactor *
        hintScoreFactor *
        bombsFactor *
        handClogFactor
      }
      val raw = {
        if(rules.stopEarlyLoss)
          game.numPlayed + maxPlaysLeft * totalFactor
        else
          game.numPlayed + maxPlaysLeft * totalFactor - scoreDropPerLostPoint * lossGap
      }

      val eval = transformEval(raw)

      if(debugging(game)) {
        println("PotentHnt: %.2f, GoodKnow: %.2f, Fixup: %.2f, NetHnt: %.2f, HSF: %.3f".format(
          numPotentialHints,goodKnowledge,fixupHintsRequired,netFreeHints,hintScoreFactor))
        println("TurnsWPossPlayLeft: %d, TWPPLF: %.3f".format(
          turnsWithPossiblePlayLeft, turnsWithPossiblePlayLeft))
        println("DangerCount: %d, DF: %.3f".format(
          dangerCount, dangerFactor))
        println("BombsLeft: %d, BF: %.3f".format(
          bombsLeft, bombsFactor))
        println("HandClogF: %.3f".format(
          handClogFactor))
        println("PlaysLeft: %d, LossGap %d, TotalFactor: %.3f".format(
          maxPlaysLeft, lossGap, totalFactor))
        println("Eval: %s".format(
          evalToString(eval)))
      }

      eval
    }
  }

  //Called at the start of the game once
  def doHandleGameStart(game: Game): Unit = {
    updateSeenMap(game)
  }


  //Update player for a given action. Return true if game still appears consistent, false otherwise.
  def doHandleSeenAction(sa: SeenAction, postGame: Game): Boolean = {
    updateSeenMap(postGame)
    sa match {
      case (sd: SeenDiscard) =>
        handleSeenDiscard(sd,postGame)
      case (sp: SeenPlay) =>
        handleSeenPlay(sp,postGame)
      case (sb: SeenBomb) =>
        handleSeenBomb(sb,postGame)
      case (sh: SeenHint) =>
        handleSeenHint(sh,postGame)
    }

    val consistent = checkIsConsistent(postGame)
    if(consistent)
      simplifyBeliefs(postGame)
    consistent
  }

  //Perform the given action assuming the given CardIds are the given Cards, and recursively search and evaluate the result.
  //Assumes other players act "simply", according to evalLikelyActionSimple
  //At the end, restore to the saved state.
  def tryAction(game: Game, ga: GiveAction, assumingCards: List[(CardId,Card)], cDepth: Int, rDepth: Int, saved: SavedState): Double = {
    val gameCopy = Game(game)
    assumingCards.foreach { case (cid,card) => gameCopy.seenMap(cid) = card }

    val sa = gameCopy.seenAction(ga)
    gameCopy.doAction(ga)

    //We need to check consistency in case the act of doing the action makes a higher-order deduction clear that we hadn't deduced
    //before that the position is actually impossible, since our logical inferencing isn't 100% complete.
    //doHandleSeenAction returns whether it finds things to be consistent or not.
    val consistent = doHandleSeenAction(sa, gameCopy)

    if(!consistent) {
      restoreState(saved)
      Double.NaN
    }
    else {
      val newCDepth = cDepth+1
      val newRDepth = rDepth-1
      val eval = {
        if(newRDepth <= 0)
          staticEvalGame(gameCopy)
        else {
          if(gameCopy.curPlayer == myPid) {
            val (_,eval) = doGetAction(gameCopy,newCDepth,newRDepth)
            eval
          }
          else
            evalLikelyActionSimple(gameCopy,newCDepth,newRDepth)
        }
      }
      restoreState(saved)
      if(debugging(gameCopy)) {
        println("Tried %-10s Assuming %s Eval: %s".format(
          game.giveActionToString(ga),
          assumingCards.map({case (_,card) => card.toString(useAnsiColors=true)}).mkString(""),
          evalToString(eval)
        ))
      }
      eval
    }
  }

  def average[T](list: List[T])(f: (T,Double) => Double): Double = {
    var sum = 0.0
    var weightSum = 0.0

    list.foreach { elt =>
      val weight = 1.0
      val eval = f(elt,weight)
      if(!eval.isNaN()) {
        sum += eval * weight
        weightSum += weight
      }
    }
    sum / weightSum
  }

  def weightedAverage[T](list: List[(T,Double)])(f: (T,Double) => Double): Double = {
    var sum = 0.0
    var weightSum = 0.0

    list.foreach { case (elt,weight) =>
      val eval = f(elt,weight)
      if(!eval.isNaN()) {
        sum += eval * weight
        weightSum += weight
      }
    }
    sum / weightSum
  }

  //Recursively evaluate averaging over a prediction of what the next player might do.
  def evalLikelyActionSimple(game: Game, cDepth: Int, rDepth: Int): Double = {
    val saved = saveState()
    val nextActions = likelyActionsSimple(game.curPlayer,game,saved)
    weightedAverage(nextActions) { (nextAction,prob) =>
      val eval = tryAction(game,nextAction,List(),cDepth,rDepth,saved)
      if(debugging(game)) {
        println("Likely next: %-12s Weight: %.3f Eval: %s".format(
          game.giveActionToString(nextAction),
          prob,
          evalToString(eval)
        ))
      }
      eval
    }
  }

  //TODO make this better

  //Returns a probability distribution on possible actions the next player might do. Modifies the state to
  //reflect what that player sees, restoring to the given state afterwards.
  def likelyActionsSimple(pid: PlayerId, game: Game, saved: SavedState): List[(GiveAction,Double)] = {
    updateSeenMap(game.hiddenFor(pid))
    val actions = {
      val playsNow: List[HandId] = expectedPlays(pid, game, now=true, ck=false)
      //Play if possible
      if(playsNow.nonEmpty)
        List((GivePlay(playsNow.head),1.0))
      //Give a hint if at max hints //TODO improve this for the last round
      else if(game.numHints >= rules.maxHints)
        List((GiveHint((pid+1) % game.rules.numPlayers, UnknownHint),1.0))
      //No hints, must discard
      else if(game.numHints <= 0) {
        val (mld,dg) = mostLikelyDiscard(pid,game,ck=false)
        //But a discard kills us - so play the first possibly playable card
        if(rules.stopEarlyLoss && (game.numDiscarded >= rules.maxDiscards || dg <= DISCARD_USEFUL)) {
          val hid = firstPossiblyPlayableHid(game,pid,ck=true).getOrElse(0)
          List((GivePlay(hid),1.0))
        }
        //Discard doesn't kill us
        else {
          List((GiveDiscard(mld),1.0))
        }
      }
      //Neither max nor no hints
      else {
        //Discard kills us - then give a hint //TODO improve this for the last round
        val (mld,dg) = mostLikelyDiscard(pid,game,ck=false)
        if(rules.stopEarlyLoss && (game.numDiscarded >= rules.maxDiscards || dg <= DISCARD_USEFUL)) {
          List((GiveHint((pid+1) % game.rules.numPlayers, UnknownHint),1.0))
        }
        else {
          //TODO pretty inaccurate, make this smarter. Note though that the evaluation
          //underestimates how good UnknownHint is because it doesn't do anything!
          //TODO why is this only possible at such a low value?
          //Assign a 2% probability to giving a hint
          List(
            // (GiveDiscard(mld),1.0)
            (GiveDiscard(mld),0.98),
            (GiveHint((pid+1) % game.rules.numPlayers, UnknownHint),0.02)
          )
        }
      }
    }
    restoreState(saved)
    actions
  }

  //Get an action for this player in the current game state via a short search.
  //rDepth is the remaining number of turns until we evaluate.
  //Returns the action and its evaluation.
  def doGetAction(game: Game, cDepth: Int, rDepth: Int): (GiveAction,Double) = {
    assert(myPid == game.curPlayer)
    val nextPid = (myPid+1) % rules.numPlayers

    //We loop over all "reasonable" actions, computing their values, using these variables
    //to store the best one, which we return at the end.
    var bestAction: GiveAction = GivePlay(0) //always legal
    var bestActionValue: Double = -10000.0
    val saved = saveState()

    def recordAction(ga: GiveAction, value: Double) = {
      if(!value.isNaN() && value > bestActionValue) {
        bestActionValue = value
        bestAction = ga
      }
      if(debugging(game)) {
        println("Action %-10s Eval: %s".format(
          game.giveActionToString(ga),
          evalToString(value)
        ))
      }
    }

    //Try all play actions
    val playsNow: List[HandId] = expectedPlays(myPid, game, now=true, ck=false)
    playsNow.foreach { hid =>
      //Right now, we only play cards we think are probably playable, so get all the possibilities
      //and filter down conditioning on the card being playable, and average over the results
      val cid = game.hands(myPid)(hid)
      val possibles = possibleCards(cid,ck=false).filter { card => game.isPlayable(card) }
      val ga = GivePlay(hid)

      val value = average(possibles) { (card,_) =>
        tryAction(game, ga, assumingCards=List((cid,card)), cDepth, rDepth, saved)
      }
      recordAction(ga,value)
    }

    //Try playing our first possibly-playable-card
    if(playsNow.isEmpty) {
      firstPossiblyPlayableHid(game,myPid,ck=false) match {
        case None => ()
        case Some(hid) =>
          val cid = game.hands(myPid)(hid)
          val possibles = possibleCards(cid,ck=false).map { card =>
            //Weight nonplayable cards ultra-heavily, so that we'll only do this as a last resort.
            //TODO can we decrease the weight?
            if(!game.isPlayable(card)) (card,100.0)
            else (card,1.0)
          }
          val ga = GivePlay(hid)
          val value = weightedAverage(possibles) { (card,_) =>
            tryAction(game, ga, assumingCards=List((cid,card)), cDepth, rDepth, saved)
          }
          recordAction(ga,value)
      }
    }

    //Try our most likely discard action
    if(game.numHints < rules.maxHints) {
      val (mld,dg) = mostLikelyDiscard(myPid,game,ck=false)
      val cid = game.hands(myPid)(mld)

      //TODO technically this and many other places that use probability distributions or weighted
      //card distributions don't handle multiplicity of cards properly in the prior - weights are not
      //affected by whether there are 1, 2, or 3 of a card left

      //Based on what kind of discard it is, reweight the various cards that it could logically be
      //to reflect that for better discards, it's rather unlikely for us to throw away something bad.
      val possiblesAndWeights = dg match {
        case (DISCARD_PROVABLE_JUNK | DISCARD_JUNK) =>
          possibleCards(cid,ck=false).map { card =>
            if(game.isJunk(card)) (card,1.0)
            else if(!game.isDangerous(card)) (card,0.1)
            else (card,0.01)
            }
        case (DISCARD_REGULAR) =>
          possibleCards(cid,ck=false).map { card =>
            if(game.isJunk(card)) (card,1.0)
            else if(!game.isDangerous(card)) (card,0.7)
            else (card,0.02) //TODO this should depend on hand position
          }
        case (DISCARD_USEFUL | DISCARD_PLAYABLE | DISCARD_MAYBE_GAMEOVER | DISCARD_GAMEOVER) =>
          possibleCards(cid,ck=false).map { card => (card,1.0) }
      }

      //Compute the average eval weighted by the weight of each card it could be.
      val ga = GiveDiscard(mld)
      val value = weightedAverage(possiblesAndWeights) { (card,_) =>
        tryAction(game, ga, assumingCards=List((cid,card)), cDepth, rDepth, saved)
      }
      recordAction(ga,value)

      //Try discarding each of our playables
      playsNow.foreach { hid =>
        if(hid != mld) {
          val cid = game.hands(myPid)(hid)
          val possiblesAndWeights = possibleCards(cid,ck=false).flatMap { card => if(game.isPlayable(card)) Some((card,1.0)) else None }
          val ga = GiveDiscard(hid)
          val value = weightedAverage(possiblesAndWeights) { (card,_) =>
            tryAction(game, ga, assumingCards=List((cid,card)), cDepth, rDepth, saved)
          }
          recordAction(ga,value)
        }
      }
    }

    //Try all hint actions
    if(game.numHints > 0) {
      (0 to rules.numPlayers-2).foreach { pidOffset =>
        possibleHintTypes.foreach { hint =>
          val ga = GiveHint((nextPid+pidOffset) % rules.numPlayers,hint)
          if(game.isLegal(ga)) {
            val value = tryAction(game, ga, assumingCards=List(), cDepth, rDepth, saved)
            recordAction(ga,value)
          }
        }
      }
    }

    //And return the best action
    (bestAction,bestActionValue)
  }

  def maybePrintAllBeliefs(game: Game): Unit = {
    if(debugging(game)) {
      (0 to rules.numPlayers-1).foreach { pid =>
        game.hands(pid).foreach { cid =>
          val card = seenMap(cid)
          println("P%d Card %s (#%d) Belief %s".format(
            pid,
            card.toString(useAnsiColors=true),
            cid,
            primeBelief(cid).toString()
          ))
        }
      }
    }
  }

  //INTERFACE --------------------------------------------------------------------

  override def handleGameStart(game: Game): Unit = {
    doHandleGameStart(game)
  }

  override def handleSeenAction(sa: SeenAction, postGame: Game): Unit = {
    val consistent = doHandleSeenAction(sa,postGame)
    assert(consistent)
  }

  override def getAction(game: Game): GiveAction = {
    maybePrintAllBeliefs(game)
    val (action,_eval) = doGetAction(game,cDepth=0,rDepth=2)
    action
  }

}
