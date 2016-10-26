/**
  * Color.scala
  * Colors of Hanabi cards.
  */

package fireflower
import scala.reflect.ClassTag

object Color {
  val ansiResetColor = "\u001B[0m"

  //Global cap on how big color ids are allowed to be - all ids are LESS than this.
  //Used for array size bounds and such.
  val LIMIT: Int = 6
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
      case MultiColor => "M"
      case NullColor => "?"
    }
  }

  def toString(useAnsiColors: Boolean): String = {
    toAnsiColorCode() + toString() + Color.ansiResetColor
  }

  def toAnsiColorCode(): String = {
    this match {
      case Red => "\u001B[31m"
      case Green => "\u001B[32m"
      case Blue => "\u001B[34m"
      case Yellow => "\u001B[33m"
      case White => ""
      case MultiColor => "\u001B[35m"
      case NullColor => "\u001B[37m"
    }
  }
}

case object Red extends Color { val id = 0 }
case object Yellow extends Color { val id = 1 }
case object Green extends Color { val id = 2 }
case object Blue extends Color { val id = 3 }
case object White extends Color { val id = 4 }
case object MultiColor extends Color { val id = 5 }
case object NullColor extends Color { val id = -1 }
