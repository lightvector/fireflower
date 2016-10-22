package fireflower


//Many of the following come in pairs, one shared between all cards that were involved
//in the same event, and a second that contains the former as a field that actually
//is the value that is tracked per-card.
//Arrays are read-only

//The shared information between all cards connected in a belief
sealed trait BeliefInfo
//The value we track per-card that stays attached to card as it moves in a hand.
sealed trait Belief

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

object HeuristicPlayer extends PlayerGen {
  def apply(rules: Rules): HeuristicPlayer = {
    val numCardsInitial = Array.fill(Card.maxArrayIdx)(0)
    rules.cards().foreach { card =>
      numCardsInitial(card.arrayIdx) += 1
    }
    new HeuristicPlayer(
      rules = rules,
      possibleHintTypes = rules.possibleHintTypes(),
      maxHints = rules.maxHints,
      uniqueCards = rules.cards().distinct.toList,
      numCardsInitial = numCardsInitial,
      seenMap = SeenMap.empty(rules),
      seenMapCK = SeenMap.empty(rules),
      hintedMap = CardPropertyMap(rules),
      beliefMap = CardPropertyMap(rules),
      colors = rules.colors()
    )
  }

  def apply(that: HeuristicPlayer): HeuristicPlayer = {
    new HeuristicPlayer(
      rules = that.rules,
      possibleHintTypes = that.possibleHintTypes.clone(),
      maxHints = that.maxHints,
      uniqueCards = that.uniqueCards,
      numCardsInitial = that.numCardsInitial,
      seenMap = SeenMap(that.seenMap),
      seenMapCK = SeenMap(that.seenMapCK),
      hintedMap = CardPropertyMap(that.hintedMap),
      beliefMap = CardPropertyMap(that.beliefMap),
      colors = that.colors
    )
  }

  def genPlayers(rules: Rules, seed: Long): Array[Player] = {
    (0 to (rules.numPlayers-1)).map { myPid =>
      this(rules)
    }.toArray
  }
}

class HeuristicPlayer private (
  val rules: Rules,
  val possibleHintTypes: Array[GiveHintType],
  val maxHints: Int,
  val uniqueCards: List[Card],
  val numCardsInitial: Array[Int],
  val colors: Array[Color],

  //Tracks what cards are visible by us
  var seenMap: SeenMap,
  //Tracks what cards are visible as common knowledge
  var seenMapCK: SeenMap,

  //Logical information we've received via hints, tracked by card
  val hintedMap: CardPropertyMap[Hinted],
  //Beliefs we have about cards based on observations
  val beliefMap: CardPropertyMap[Belief]
) extends Player {

  def debugging(game: Game): Boolean = {
    game.debugPath match {
      case None => false
      case Some(_) => true
    }
  }

  //Update the seen maps based on a new incoming game state
  def updateSeenMap(game: Game): Unit = {
    seenMap = SeenMap(game.seenMap)
    seenMapCK = SeenMap(game.seenMap)
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

  //What cards could [cid] be as a strictly logical possiblity?
  //If ck is false, uses all information known.
  //If ck is true, uses only common knowledge information.
  def possibleCards(cid: CardId, ck: Boolean): List[Card] = {
    var sm = seenMap
    if(ck) sm = seenMapCK

    val seenCard = sm(cid)
    if(seenCard != Card.NULL)
      List(seenCard)
    else {
      val possibles: List[Card] = sm.uniqueUnseen()
      hintedMap(cid).foldLeft(possibles) { case (possibles,hinted) =>
        possibles.filter { card => rules.isConsistent(hinted.info.sh.hint, hinted.applied, card) }
      }
    }
  }

  def provablyPlayable(possibles: List[Card], game: Game): Boolean = {
    possibles.forall { card => game.isPlayable(card) }
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

  //If beliefs and conventions strongly suggest that this card should be a specific card, return
  //that card, otherwise return Card.NULL.
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

  //Goodness and discard are by common knowledge if and only if
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

  //Find all the hand positions we think the given player can play.
  //Now determines if the cards must be playable now rather than eventually.
  //ck determines if we only check using common knowledge or all observed info
  def possiblePlays(pid: PlayerId, game: Game, now: Boolean, ck: Boolean): List[HandId] = {
    val hand = game.hands(pid)
    (0 to (hand.numCards-1)).filter { hid =>
      val cid = hand(hid)
      val possibles = possibleCards(cid,ck)
      if(provablyNotPlayable(possibles,game))
        false
      else {
        provablyPlayable(possibles,game) ||
        {
          primeBelief(cid) match {
            case None => false
            case Some(_: ProtectedSet) => false
            case Some(_: JunkSet) => false
            case Some(b: PlaySequence) =>
              if(now) b.seqIdx == 0 else true
          }
        }
      }
    }.toList
  }

  override def handleGameStart(game: Game): Unit = {
    updateSeenMap(game)
  }

  override def handleSeenAction(sa: SeenAction, postGame: Game): Unit = {
    updateSeenMap(postGame)
    sa match {
      case SeenDiscard(hid,cid) =>
        val card = seenMap(cid)

        //Check and update beliefs based on the discard if the discard turned out to not actually be junk
        if(card != Card.NULL && !postGame.isJunk(card)) {
          primeBelief(cid) match {
            //No prior belief
            case None => ()
            //Card was protected - something unusual must have happened for the player to throw it away
            //TODO we should probably have some logic here
            case Some(_: ProtectedSet) => ()
            //Card was a believed play - something unusual must have happened for the player to throw it away
            //TODO we should probably have some logic here
            case Some(_: PlaySequence) => ()
            //Card was believed junk
            case Some(b: JunkSet) =>
              //If the card was not actually junk, then immediately change everything in the believed junk set
              //to protected so we don't keep discarding them.
              addBelief(ProtectedSetInfo(cids = b.info.cids))
          }
        }

      case SeenPlay(hid,cid) =>
        val card = seenMap(cid)
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

      case SeenBomb(hid,cid) =>
        //Unsuccessful play
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
            if(postGame.isUseful(card)) {
              addBelief(ProtectedSetInfo(cids = b.info.cids))
            }
        }

      case (sh: SeenHint) =>
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
        val prePossiblePlays: List[HandId] = possiblePlays(pid,postGame,now=true,ck=true)

        //See what card that player would have been likely to discard
        val (preMLD,preMLDGoodness): (HandId, DiscardGoodness) = mostLikelyDiscard(pid,postGame,ck=true)

        //Now update hintedMap with the logical information of the hint
        val hintedInfo = HintedInfo(sh, hand.cardArray())
        for (hid <- 0 to (hand.numCards-1)) {
          val hinted = Hinted(hid,sh.appliedTo(hid),hintedInfo)
          hintedMap.add(hand(hid),hinted)
        }

        //Check if it's a number hint where all cards touched are possibly playable and the number of cards
        //touched is larger than the number of playables or eventual playables of that number.
        val numberHintWithPlay: Boolean = {
          sh.hint match {
            case HintNumber(num) =>
              val allPossiblyPlayble = hintCids.forall { cid =>
                val possibles = possibleCards(cid,ck=true)
                !provablyNotPlayable(possibles,postGame)
              }
              allPossiblyPlayble && hintCids.length > postGame.nextPlayable.count { n => n <= num }
            case _ => false
          }
        }

        //If the hint targets the most likely discard
        //AND (there are no cards that the player would have played OR the hint touches a card we would have played)
        //AND (it's not a number hint where common knowledge says at least one MUST be playable)
        //AND (it's possible that the mld is a dangerous card after this hint)
        //then it's a protection hint.
        if(sh.appliedTo(preMLD) &&
          (prePossiblePlays.isEmpty || prePossiblePlays.exists { hid => sh.appliedTo(hid) }) &&
          !numberHintWithPlay &&
          !provablyNotDangerous(possibleCards(preMLD,ck=true),postGame)
        ) {
          addBelief(ProtectedSetInfo(cids = hintCids))
        }
        //TODO this needs to be more sophisticated as well
        //Otherwise if at least one card could be playable after the hint, then it's a play hint
        else if(hintCids.exists { cid =>
          val possibles = possibleCards(cid,ck=true)
          !provablyNotPlayable(possibles,postGame)
        }) {
          //TODO this needs to be more sophisticated and take into account other hinted-as-plays cards
          //Cards that are provably playable come first in the ordering
          val (hintCidsProvable, hintCidsNotProvable): (Array[CardId],Array[CardId]) =
            hintCids.partition { cid => provablyPlayable(possibleCards(cid,ck=true),postGame) }
          addBelief(PlaySequenceInfo(cids = hintCidsProvable ++ hintCidsNotProvable))
        }
        //Otherwise if all cards in the hint are provably unplayable and not provably junk,
        //then it's a protection hint.
        else if(hintCids.forall { cid =>
          val possibles = possibleCards(cid,ck=true)
          provablyNotPlayable(possibles,postGame) && !provablyJunk(possibles,postGame)
        }) {
          addBelief(ProtectedSetInfo(cids = hintCids))
        }
    }

    //Find all card ids in a hand right now
    val cidInHandAndCouldBePlayable: Array[Boolean] = Array.fill(rules.deckSize)(false)
    postGame.hands.foreach { hand =>
      hand.foreach { cid =>
        val possibles = possibleCards(cid,ck=true)
        if(!provablyNotPlayable(possibles,postGame))
          cidInHandAndCouldBePlayable(cid) = true
      }
    }
    //Filter play sequences down to only these card ids
    postGame.hands.foreach { hand =>
      hand.foreach { cid =>
        primeBelief(cid) match {
          case None => ()
          case Some(_: ProtectedSet) => ()
          case Some(_: JunkSet) => ()
          case Some(b: PlaySequence) =>
            val (newCids,filteredCids) = b.info.cids.partition { cid => cidInHandAndCouldBePlayable(cid) }
            if(filteredCids.length > 0) {
              addBelief(PlaySequenceInfo(cids = newCids))
              addBelief(ProtectedSetInfo(cids = filteredCids))
            }
        }
      }
    }

  }


  def softPlus(x: Double, width: Double) = {
    if(x/width >= 40.0)
      40.0
    else
      Math.log(1.0 + Math.exp(x/width)) * width
  }

  def staticEvalGame(game: Game): Double = {
    if(game.isDone())
      game.numPlayed.toDouble
    else {
      val numDiscardsLeft = rules.maxDiscards - game.numDiscarded
      val numPotentialHints =
        numDiscardsLeft + game.numHints + {
          if(rules.extraHintFromPlayingMax)
            colors.count { color => game.nextPlayable(color.id) <= rules.maxNumber }
          else
            0
        }
      //TODO these don't help much or they hurt!
      val fixupHintsRequired = 0.0
        // game.hands.foldLeft(0.0) { case (acc,hand) =>
        //   hand.foldLeft(0.0) { case (acc,cid) =>
        //     val card = game.seenMap(cid)
        //     val value = {
        //       if(card == Card.NULL)
        //         0.0
        //       else {
        //         val possibles = possibleCards(cid,ck=true)
        //         if(!provablyNotPlayable(possibles,game) && isBelievedPlayable(cid,now=true) && !game.isPlayable(card))
        //           0.8
        //         if(!provablyJunk(possibles,game) && isBelievedPlayable(cid,now=false) && !game.isUseful(card))
        //           0.6
        //         else if(!provablyJunk(possibles,game) && isBelievedUseful(cid) && !game.isUseful(card))
        //           0.4
        //         else if(!provablyUseful(possibles,game) && isBelievedJunk(cid) && game.isDangerous(card))
        //           0.8
        //         else
        //           0.0
        //       }
        //     }
        //     //TODO this is buggy due to not adding acc
        //     value
        //   }
        // }
      val goodKnowledge = 0.0
        // game.hands.foldLeft(0.0) { case (acc,hand) =>
        //   hand.foldLeft(0.0) { case (acc,cid) =>
        //     val card = game.seenMap(cid)
        //     val value = {
        //       if(isBelievedPlayable(cid,now=true) && (card == Card.NULL || game.isPlayable(card)))
        //         0.8
        //       //TODO make this better
        //       else if(!isBelievedPlayable(cid,now=true) && isBelievedPlayable(cid,now=false) &&
        //         (card == Card.NULL || (!game.isPlayable(card) && game.isUseful(card))))
        //         0.8
        //       else if(isBelievedUseful(cid) && (card != Card.NULL && game.isDangerous(card)))
        //         0.6
        //       else if(isBelievedUseful(cid) && (card == Card.NULL || game.isUseful(card)))
        //         0.3
        //       else if(isBelievedJunk(cid) && (card == Card.NULL || game.isJunk(card)))
        //         0.1
        //       else
        //         0.0
        //     }
        //     //TODO this is buggy due to not adding acc
        //     value
        //   }
        // }

      val playsLeft = rules.maxScore - game.numPlayed
      val netFreeHints = numPotentialHints * 0.9 + goodKnowledge - (fixupHintsRequired + playsLeft) - 2

      //How much of the remaining score are we not getting due to lack of hints
      val hintScoreFactor = Math.max(0, (playsLeft.toDouble + 3.0 - softPlus(-netFreeHints,3.0)) / (playsLeft + 3.0))

      //How much of the remaining score are we not getting due to lack of turns
      val turnsLeft = {
        if(game.finalTurnsLeft >= 0) game.finalTurnsLeft
        else game.deck.length + rules.numPlayers
      }
      val turnsLeftFactor = Math.min(playsLeft.toDouble, 0.8 * turnsLeft) / playsLeft.toDouble

      //How much of the remaining score are we not getting due to danger stuff
      val dangerCount = uniqueCards.foldLeft(0) { case (acc,card) =>
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

      val handClogFactor = game.hands.foldLeft(1.0) { case (acc,hand) =>
        val numClogs = hand.count { cid =>
          val card = game.seenMap(cid)
          if(card != Card.NULL && game.isPlayable(card))
            false
          else if(card != Card.NULL && game.isDangerous(card))
            true
          else if(isBelievedUseful(cid) && card != Card.NULL && !game.isPlayable(card))
            true
          else (isBelievedUseful(cid) && !isBelievedPlayable(cid,now=false))
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

      val bombsLeft = rules.maxBombs - game.numBombs
      val bombsFactor = {
        if(bombsLeft >= 3) 1.0
        else if(bombsLeft == 2) 0.98
        else if(bombsLeft == 1) 0.93
        else 0.0
      }

      val total =
        game.numPlayed + (rules.maxScore - game.numPlayed) * dangerFactor * turnsLeftFactor * hintScoreFactor * bombsFactor * handClogFactor
      val expedTotal = Math.exp(total / 3.0)

      if(debugging(game)) {
        println("PotentHnt: %d, GoodKnow: %.2f, Fixup: %.2f, NetHnt: %.2f, HSF: %.3f".format(
          numPotentialHints,goodKnowledge,fixupHintsRequired,netFreeHints,hintScoreFactor))
        println("TurnsLeft: %d, TLF: %.3f".format(
          turnsLeft, turnsLeftFactor))
        println("DangerCount: %d, DF: %.3f".format(
          dangerCount, dangerFactor))
        println("BombsLeft: %d, BF: %.3f".format(
          bombsLeft, bombsFactor))
        println("ScoreLeft: %d, Total: %.3f, Exped: %.3f".format(
          rules.maxScore - game.numPlayed, total, expedTotal))
      }

      expedTotal
    }
  }

  def evalActions(game: Game, actions: List[GiveAction], assumingCards: List[(CardId,Card)]): Double = {
    val gameCopy = Game(game)
    val playerCopy = HeuristicPlayer(this)
    assumingCards.foreach { case (cid,card) => gameCopy.seenMap(cid) = card}
    actions.foreach { ga =>
      val sa = gameCopy.seenAction(ga)
      gameCopy.doAction(ga)
      playerCopy.handleSeenAction(sa, gameCopy)
    }
    playerCopy.staticEvalGame(gameCopy)
  }

  def evalLikelyActionSimple(nextPid: PlayerId, game: Game, ga: GiveAction, assumingCards: List[(CardId,Card)]): Double = {
    val nextPlayer = HeuristicPlayer(this)
    val nextGame = Game(game)
    assumingCards.foreach { case (cid,card) => nextGame.seenMap(cid) = card }
    val sa = nextGame.seenAction(ga)
    nextGame.doAction(ga)
    nextPlayer.handleSeenAction(sa, nextGame.hiddenFor(nextPid))
    val nextAction = nextPlayer.likelyActionSimple(nextPid,nextGame)
    val eval = evalActions(game,List(ga,nextAction),assumingCards)

    if(debugging(game)) {
      println("Action " + game.giveActionToString(ga) +
        " assume " + (assumingCards.map(_._2).map(_.toString(useAnsiColors=true)).mkString("")) +
        " likely next: " + game.giveActionToString(nextAction) + " Eval: " + eval)
    }
    eval
  }

  //TODO make this better
  def likelyActionSimple(pid: PlayerId, game: Game): GiveAction = {
    val playsNow: List[HandId] = possiblePlays(pid, game, now=true, ck=false)
    if(playsNow.nonEmpty)
      GivePlay(playsNow.head)
    else {
      val (mld,dg) = mostLikelyDiscard(pid,game,ck=false)
      GiveDiscard(mld)
    }
  }

  override def getAction(game: Game): GiveAction = {
    val myPid = game.curPlayer
    val nextPid = (myPid+1) % rules.numPlayers

    val plays: List[HandId] = possiblePlays(myPid, game, now=false, ck=false)
    val playsNow: List[HandId] = possiblePlays(myPid, game, now=true, ck=false)

    var bestAction: GiveAction = GiveDiscard(0)
    var bestActionValue: Double = -10000.0

    def recordAction(ga: GiveAction, value: Double) = {
      if(value > bestActionValue) {
        bestActionValue = value
        bestAction = ga
      }
      if(debugging(game)) {
        println("Action " + game.giveActionToString(ga) + " eval " + value)
      }
    }

    //Try all play actions
    playsNow.foreach { hid =>
      //Right now, we only play cards we think are probably playable, so get all the possibilities
      //and filter down conditioning on the card being playable, and average over the results
      val cid = game.hands(myPid)(hid)
      val possibles = possibleCards(cid,ck=false).filter { card => game.isPlayable(card) }
      val ga = GivePlay(hid)
      var sum = 0.0
      val evals = possibles.foreach { card =>
        sum += evalLikelyActionSimple(nextPid,game,ga,assumingCards=List((cid,card)))
      }
      val value = sum / possibles.length.toDouble
      recordAction(ga,value)
    }

    //Try our most likely discard action
    if(game.numHints < rules.maxHints) {
      val (mld,dg) = mostLikelyDiscard(myPid,game,ck=false)
      val cid = game.hands(myPid)(mld)
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
            else (card,0.02)
          }
        case (DISCARD_USEFUL | DISCARD_PLAYABLE) =>
          possibleCards(cid,ck=false).map { card =>
            if(!game.isDangerous(card)) (card,1.0)
            else (card,0.1)
          }
        case (DISCARD_MAYBE_GAMEOVER | DISCARD_GAMEOVER) =>
          possibleCards(cid,ck=false).map { card => (card,1.0) }
      }
      val ga = GiveDiscard(mld)
      var sum = 0.0
      var wsum = 0.0
      val evals = possiblesAndWeights.foreach { case (card,weight) =>
        sum += weight * evalLikelyActionSimple(nextPid,game,ga,assumingCards=List((cid,card)))
        wsum += weight
      }
      val value = sum / wsum
      recordAction(ga,value)
    }

    //Try all hint actions
    if(game.numHints > 0) {
      possibleHintTypes.foreach { hint =>
        val ga = GiveHint(nextPid,hint)
        if(game.isLegal(ga)) {
          val value = evalLikelyActionSimple(nextPid,game,ga,assumingCards=List())
          recordAction(ga,value)
        }
      }
    }

    //And return the best action
    bestAction
  }

}
