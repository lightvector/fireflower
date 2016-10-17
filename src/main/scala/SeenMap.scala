/**
  * SeenMap.scala
  * A basic mutable structure that maps CardId => Card, or to Card.NULL, representing which cards are visible
  * and which cards are not visible. Also keeps counts of the number of unseen cards of each type.
  */

package fireflower

object SeenMap {
  def apply(rules: Rules, rand: Rand): SeenMap = {
    val cards = rules.cards()
    rand.shuffle(cards)
    new SeenMap(
      cards = cards,
      numUnseenByCard = Array.fill(Card.maxArrayIdx)(0),
      numUnseen = 0,
      distinctCards = cards.distinct
    )
  }
  def apply(that: SeenMap): SeenMap = {
    new SeenMap(
      cards = that.cards.clone(),
      numUnseenByCard = that.numUnseenByCard.clone(),
      numUnseen = that.numUnseen,
      distinctCards = that.distinctCards.clone()
    )
  }
  def empty(rules: Rules): SeenMap = {
    val cards = rules.cards()
    val numUnseenByCard = Array.fill(Card.maxArrayIdx)(0)
    cards.foreach { card => numUnseenByCard(card.arrayIdx) += 1 }
    new SeenMap(
      cards = Array.fill(rules.deckSize)(Card.NULL),
      numUnseenByCard = numUnseenByCard,
      numUnseen = rules.deckSize,
      distinctCards = cards.distinct
    )
  }
}

class SeenMap private (
  //Maps CardId -> Card, or Card.NULL if not known
  val cards: Array[Card],
  //Count of number of unseen cards, indexed by card.arrayIdx
  val numUnseenByCard: Array[Int],
  //Total number of unseen cards
  var numUnseen: Int,
  //Unique cards in the deck
  val distinctCards: Array[Card]
) {

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
      numUnseenByCard(oldCard.arrayIdx) += 1
    }
    cards(cid) = card
    if(card != Card.NULL) {
      numUnseenByCard(card.arrayIdx) -= 1
    }
  }

  //Get a list of the unique cards that have at least one unseen
  def uniqueUnseen(): List[Card] = {
    (0 to (distinctCards.length-1)).flatMap { i =>
      if(numUnseenByCard(distinctCards(i).arrayIdx) > 0)
        Some(distinctCards(i))
      else
        None
    }.toList
  }
}
