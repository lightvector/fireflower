/**
  * Card.scala
  * The basic type of a Hanabi card.
  * By convention, numbers of hanabi cards start at ZERO. They are incremented upon display.
  */

package fireflower
import scala.reflect.ClassTag

object Card {
  val NULL: Card = Card(NullColor,-1)

  //Global cap on how big numbers on cards are allowed to be - all numbers are LESS than this.
  //Used for array size bounds and such.
  val NUMBER_LIMIT: Int = 5

  //Maximum possible card array index
  val maxArrayIdx: Int = Card.NUMBER_LIMIT * Color.LIMIT

  def arrayIdx(color: Color, number: Int): Int = {
    number +  Card.NUMBER_LIMIT * color.id
  }
}

case class Card(
  color: Color,
  number: Number
) extends Ordered[Card] {

  //Map this card to an array index based on its properties, for array-based card lookups.
  def arrayIdx: Int = {
    number + Card.NUMBER_LIMIT * color.id
  }

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
      if(useAnsiColors) color.toAnsiColorCode() + (number+1).toString() + Color.ansiResetColor
      else color.toString() + (number+1).toString()
    }
  }
}
