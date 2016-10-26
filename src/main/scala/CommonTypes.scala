/**
  * CommonTypes.scala
  * Some basic type aliases for convenience so that method and class signatures and are more self-documenting.
  */

package fireflower

object `package` {
  type ColorId = Int  //Integer index for different colors, ranges from 0 to (number of colors supported in this code - 1)
  type CardId = Int   //Integer id for cards in a particular game, ranges from 0 to (deck size - 1)
  type PlayerId = Int //Integer id for players in a particular game, ranges from 0 to (num players - 1)
  type HandId = Int   //Integer id for hand positions in a player's hand, ranges from 0 to (player's hand size)
  type Number = Int   //Possible numbers for a card, ranges from 0 to 4 under normal rules.
}

object CardId {
  //CardIds sometimes need a null value, such as for filling empty spots in a hand.
  //We could use an option, but this is a bit cheaper.
  val NULL = -1
}
