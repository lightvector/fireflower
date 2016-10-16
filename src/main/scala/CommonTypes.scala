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

object Color {
  val ansiResetColor = "\u001B[0m"
}
sealed trait Color extends Ordered[Color] {
  val id: ColorId
  def compare(that: Color): Int = id.compare(that.id)
  override def toString(): String = {
    this match {
      case Red => "R"
      case Yellow => "Y"
      case Green => "G"
      case Blue => "B"
      case White => "W"
      case Rainbow => "Z"
      case NullColor => "?"
    }
  }
  def toAnsiColorCode(): String = {
    this match {
      case Red => "\u001B[31m"
      case Green => "\u001B[32m"
      case Blue => "\u001B[34m"
      case Yellow => "\u001B[33m"
      case White => ""
      case Rainbow => "\u001B[35m"
      case NullColor => "\u001B[37m"
    }
  }
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

  def toString(useAnsiColors: Boolean): String = {
    if(this == Card.NULL)
    {
      if(useAnsiColors) "?"
      else "??"
    }
    else {
      if(useAnsiColors) color.toAnsiColorCode() + number.toString() + Color.ansiResetColor
      else color.toString() + number.toString()
    }
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

sealed trait GiveAction
case class GiveDiscard(hid: HandId) extends GiveAction
case class GivePlay(hid: HandId) extends GiveAction
case class GiveHint(pid: PlayerId, hint: GiveHintType) extends GiveAction

sealed trait SeenAction
case class SeenDiscard(hid: HandId, cid: CardId) extends SeenAction
case class SeenPlay(hid: HandId, cid: CardId) extends SeenAction
case class SeenBomb(hid: HandId, cid: CardId) extends SeenAction
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
    cards(numCards) = CardId.NULL
    removed
  }

  def exists(f: CardId => Boolean): Boolean = {
    cards.exists(f)
  }

  def foreach(f: CardId => Unit): Unit = {
    cards.foreach(f)
  }

  def mapCards[T:ClassTag](f: CardId => T): Array[T] = {
    Array.tabulate[T](numCards) { i =>
      f(cards(numCards-i-1))
    }
  }

  def toString(cardMap: CardMap, useAnsiColors: Boolean): String = {
    cards.reverse.flatMap { cid =>
      if(cid == CardId.NULL)
        None
      else
        Some(cardMap(cid).toString(useAnsiColors))
    }.mkString("")
  }
}

object CardMap {
  def apply(rules: Rules, rand: Rand): CardMap = {
    val cards = rules.cards()
    rand.shuffle(cards)
    new CardMap(
      cards = cards,
      numUnknownByCard = Array.fill(rules.maxNumber * (rules.maxColorId+1))(0),
      numUnknown = 0,
      maxNumber = rules.maxNumber
    )
  }
  def apply(that: CardMap): CardMap = {
    new CardMap(
      cards = that.cards.clone(),
      numUnknownByCard = that.numUnknownByCard.clone(),
      numUnknown = that.numUnknown,
      maxNumber = that.maxNumber
    )
  }
}

class CardMap private (
  //Maps CardId -> Card, or Card.NULL if not known
  val cards: Array[Card],
  //Count of number of unknown cards, indexed by (number-1) + maxNumber * color.id
  val numUnknownByCard: Array[Int],
  //Total number of unknown cards
  var numUnknown: Int,
  val maxNumber: Int
) {

  private def cardLookupIdx(card: Card) = {
    card.number - 1 + maxNumber * card.color.id
  }

  def apply(cid: CardId): Card = {
    cards(cid)
  }

  //Swap the cards mapped to two card ids
  def swap(c0: CardId, c1: CardId) = {
    val tmp = cards(c0)
    cards(c0) = cards(c1)
    cards(c1) = tmp
  }

  //Set the mapping of card for a card id, does not check validity
  def update(cid: CardId, card: Card) = {
    val oldCard = cards(cid)
    if(oldCard != Card.NULL) {
      numUnknownByCard(cardLookupIdx(oldCard)) += 1
    }
    cards(cid) = card
    if(card != Card.NULL) {
      numUnknownByCard(cardLookupIdx(card)) -= 1
    }
  }
}

trait Player {
  def handleGameStart(game: Game): Unit
  def getAction(game: Game): GiveAction
}
