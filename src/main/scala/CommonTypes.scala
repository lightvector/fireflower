package fireflower

import scala.reflect.ClassTag

object `package` {
  type ColorId = Int
  type CardId = Int
  type PlayerId = Int
  type HandId = Int
}

object CardId {
  val NULL = -1
}

sealed trait Color extends Ordered[Color] {
  val id: ColorId
  def compare(that: Color): Int = id.compare(that.id)
}
case object Red extends Color { val id = 0 }
case object Yellow extends Color { val id = 1 }
case object Green extends Color { val id = 2 }
case object Blue extends Color { val id = 3 }
case object White extends Color { val id = 4 }
case object Rainbow extends Color { val id = 5 }
case object NullColor extends Color { val id = 6 }

case class Card(
  color: Color,
  number: Int
) extends Ordered[Card] {

  def compare(that: Card): Int = {
    import scala.math.Ordered.orderingToOrdered
    (color,number).compare((that.color,that.number))
  }
}

object Card {
  val NULL = Card(NullColor,0)
}

//What kinds of hints may be given
sealed trait GiveHintType
//What kinds of hints a player may recieve / learn
sealed trait SeenHintType

case class HintColor(color: Color) extends SeenHintType with GiveHintType
case class HintNumber(number: Int) extends SeenHintType with GiveHintType
case object HintSameColor extends SeenHintType
case object HintSameNumber extends SeenHintType
case object HintSame extends SeenHintType

abstract class Rules {
  val numPlayers: Int
  val deckSize: Int
  val handSize: Int
  val initialHints: Int
  val maxHints: Int
  val maxBombs: Int
  val maxDiscards: Int
  val maxScore: Int

  val maxNumber: Int
  val numColors: Int
  val maxColorId: Int

  def cards(): Array[Card]
  def possibleHintTypes(): Array[GiveHintType]

  def extraHintFromPlaying(num: Int) : Boolean

  def seenHint(hint: GiveHintType): SeenHintType
  def hintApplies(hint: GiveHintType, card: Card): Boolean

  def isConsistent(hint: SeenHintType, applied: Boolean, card: Card): Boolean
}

sealed trait GiveAction
case class GiveDiscard(hid: HandId) extends GiveAction
case class GivePlay(hid: HandId) extends GiveAction
case class GiveHint(pid: PlayerId, hint: GiveHintType) extends GiveAction

sealed trait SeenAction
case class SeenDiscard(hid: HandId, cid: CardId) extends SeenAction
case class SeenPlay(hid: HandId, cid: CardId) extends SeenAction
case class SeenHint(pid: PlayerId, hint: SeenHintType, appliedTo: Array[Boolean]) extends SeenAction

//Index 0 is the newest card
object Hand {
  def apply(handSize: Int): Hand = new Hand(cards = Array.fill(handSize)(CardId.NULL), numCards = 0)
  def apply(that: Hand): Hand = new Hand(cards = that.cards.clone(), numCards = that.numCards)
}

class Hand private (
  private val cards: Array[CardId], //private, stored in reverse index order
  var numCards: Int
) {

  def apply(hid: HandId): CardId = {
    cards(numCards-hid-1)
  }

  def add(cid: CardId): Unit = {
    cards(numCards) = cid
    numCards += 1
  }

  def remove(hid: HandId): CardId = {
    var i = numCards-hid-1
    val removed = cards(i)
     while(i < numCards-1) {
      cards(i) = cards(i+1)
      i += 1
    }
    numCards -= 1
    removed
  }

  def exists(f: CardId => Boolean): Boolean = {
    cards.exists(f)
  }

  def mapCards[T:ClassTag](f: CardId => T): Array[T] = {
    Array.tabulate[T](numCards) { i => f(cards(numCards-i-1)) }
  }
}

object CardMap {
  def apply(rules: Rules, rand: Rand): CardMap = {
    val cards = rules.cards()
    rand.shuffle(cards)
    new CardMap(
      cards = cards,
      unknown = cards.map { _ => Card.NULL },
      numUnknown = 0
    )
  }
  def apply(that: CardMap): CardMap = {
    new CardMap(
      cards = that.cards.clone(),
      unknown = that.unknown.clone(),
      numUnknown = that.numUnknown
    )
  }
}

class CardMap private (
  //Maps CardId -> Card, or Card.NULL if not known
  val cards: Array[Card],
  //Array of unknown cards. Values are only valid in the range [0,numUnknown)
  val unknown: Array[Card],
  var numUnknown: Int
) {

  def apply(cid: CardId): Card = {
    cards(cid)
  }

  def hide(cid: CardId): Unit = {
    pushUnknown(cards(cid))
    cards(cid) = Card.NULL
  }

  def sortHidden(): Unit = {
    scala.util.Sorting.quickSort(cards)
  }

  def swap(c0: CardId, c1: CardId) = {
    val tmp = cards(c0)
    cards(c0) = cards(c1)
    cards(c1) = tmp
  }

  def swapUnknown(c: CardId, u:Int) = {
    val tmp = cards(c)
    cards(c) = unknown(u)
    unknown(u) = tmp
  }

  private def pushUnknown(card: Card): Unit = {
    unknown(numUnknown) = card
    numUnknown += 1
  }

}
