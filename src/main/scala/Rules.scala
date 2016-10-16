/**
  * Rules.scala
  * Defines various rule sets for Hanabi.
  * Capable of expressing a variety of sets of game parameters and also
  * variations on how hints work, including rainbow cards and such.
  */

package fireflower

abstract class Rules {
  val numPlayers: Int
  val deckSize: Int
  val handSize: Int
  val initialHints: Int
  val maxHints: Int
  val maxBombs: Int
  val maxDiscards: Int
  val maxScore: Int

  def cards(): Array[Card]
  def colors(): Array[Color]
  def possibleHintTypes(): Array[GiveHintType]

  def extraHintFromPlaying(num: Int): Boolean

  def seenHint(hint: GiveHintType): SeenHintType
  def hintApplies(hint: GiveHintType, card: Card): Boolean
  def isConsistent(hint: SeenHintType, applied: Boolean, card: Card): Boolean
}

object Rules {
  case class Standard(val numPlayers: Int) extends Rules {
    if(numPlayers < 2 || numPlayers > 5)
      throw new Exception("Standard rules do not support numPlayers < 2 or > 5")

    val deckSize = 50
    val handSize = numPlayers match {
      case 2 | 3 => 5
      case 4 | 5 => 4
    }

    val initialHints = 8
    val maxHints = 8
    val maxBombs = 2

    val maxDiscards = numPlayers match {
      case 2 => 17
      case 3 | 4 => 13
      case 5 => 10
    }

    val maxScore = 25
    val maxNumber = 4

    val colorList: List[Color] = List(Red,Yellow,Green,Blue,White)
    val maxColorId = colorList.map(color => color.id).reduceLeft(math.max)

    def colors(): Array[Color] = {
      colorList.toArray
    }

    def cards(): Array[Card] = {
      (0 to maxNumber).flatMap { number =>
        colorList.flatMap { color =>
          if(number == 0)
            List(Card(color,number),Card(color,number),Card(color,number))
          else if(number < maxNumber)
            List(Card(color,number),Card(color,number))
          else
            List(Card(color,number))
        }
      }.toArray
    }

    def possibleHintTypes(): Array[GiveHintType] = {
      colorList.map(color => HintColor(color)).toArray[GiveHintType] ++
        (0 to maxNumber).map(number => HintNumber(number)).toArray[GiveHintType]
    }

    def extraHintFromPlaying(num: Int): Boolean = {
      num == maxNumber
    }

    def seenHint(hint: GiveHintType): SeenHintType = {
      hint.asInstanceOf[SeenHintType]
    }

    def hintApplies(hint: GiveHintType, card: Card): Boolean = {
      if(card == Card.NULL)
        throw new Exception("Null card used in Game.hintApplies")
      hint match {
        case HintColor(color) =>
          card.color == color
        case HintNumber(number) =>
          card.number == number
      }
    }

    def isConsistent(hint: SeenHintType, applied: Boolean, card: Card): Boolean = {
      hint match {
        case HintColor(color) =>
          (card.color == color) == applied
        case HintNumber(number) =>
          (card.number == number) == applied
        case HintSameColor | HintSameNumber | HintSame => true
      }
    }
  }

}
