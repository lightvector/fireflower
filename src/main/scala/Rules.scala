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

  val maxNumber: Int
  val maxColorId: Int

  def cards(): Array[Card]
  def colors(): Array[Color]
  def possibleHintTypes(): Array[GiveHintType]

  def extraHintFromPlaying(num: Int): Boolean

  def seenHint(hint: GiveHintType): SeenHintType
  def hintApplies(hint: GiveHintType, card: Card): Boolean
  def isConsistent(hint: SeenHintType, applied: Boolean, card: Card): Boolean
}

object Rules {
  object StandardTwoPlayer extends Rules {
    val numPlayers = 2
    val deckSize = 50
    val handSize = 5
    val initialHints = 8
    val maxHints = 8
    val maxBombs = 2
    val maxDiscards = 17
    val maxScore = 25
    val maxNumber = 5
    val colorList: List[Color] = List(Red,Yellow,Green,Blue,White)
    val maxColorId = colorList.map(color => color.id).reduceLeft(math.max)

    def colors(): Array[Color] = {
      colorList.toArray
    }

    def cards(): Array[Card] = {
      (1 to 5).flatMap { number =>
        colorList.flatMap { color =>
          if(number == 1)
            List(Card(color,number),Card(color,number),Card(color,number))
          else if(number < 5)
            List(Card(color,number),Card(color,number))
          else
            List(Card(color,number))
        }
      }.toArray
    }

    def possibleHintTypes(): Array[GiveHintType] = {
      colorList.map(color => HintColor(color)).toArray[GiveHintType] ++
        (1 to maxNumber).map(number => HintNumber(number)).toArray[GiveHintType]
    }

    def extraHintFromPlaying(num: Int): Boolean = {
      num == 5
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
