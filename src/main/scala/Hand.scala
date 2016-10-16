/**
  * Hand.scala
  * A basic mutable structure for representing a person's hand in Hanabi
  * Index 0 is the newest card drawn.
  */

package fireflower
import scala.reflect.ClassTag

object Hand {
  def apply(handSize: Int): Hand = new Hand(cards = Array.fill(handSize)(CardId.NULL), numCards = 0)
  def apply(that: Hand): Hand = new Hand(cards = that.cards.clone(), numCards = that.numCards)
}

class Hand private (
  private val cards: Array[CardId],
  var numCards: Int
) {

  def apply(hid: HandId): CardId = {
    cards(hid)
  }

  def add(cid: CardId): Unit = {
    cards(numCards) = cid
    numCards += 1
  }

  def remove(hid: HandId): CardId = {
    var i = hid
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
    var i = 0
    var found = false
    while(i < numCards && !found) {
      found = f(cards(i))
      i += 1
    }
    found
  }

  def foreach(f: CardId => Unit): Unit = {
    var i = 0
    while(i < numCards) {
      f(cards(i))
      i += 1
    }
  }

  def mapCards[T:ClassTag](f: CardId => T): Array[T] = {
    Array.tabulate[T](numCards) { i => f(cards(i)) }
  }

  def toString(seenMap: SeenMap, useAnsiColors: Boolean): String = {
    mapCards { cid => seenMap(cid).toString(useAnsiColors) }.mkString("")
  }
}
