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
      numUnseen = 0
    )
  }
  def apply(that: SeenMap): SeenMap = {
    new SeenMap(
      cards = that.cards.clone(),
      numUnseenByCard = that.numUnseenByCard.clone(),
      numUnseen = that.numUnseen
    )
  }
}

class SeenMap private (
  //Maps CardId -> Card, or Card.NULL if not known
  val cards: Array[Card],
  //Count of number of unseen cards, indexed by (number-1) + maxNumber * color.id
  val numUnseenByCard: Array[Int],
  //Total number of unseen cards
  var numUnseen: Int
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
}
