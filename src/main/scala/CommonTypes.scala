package fireflower

sealed trait Color
case object Red extends Color
case object Yellow extends Color
case object Green extends Color
case object Blue extends Color
case object White extends Color
case object Rainbow extends Color
case object NullColor extends Color

case class Card(
  color: Color,
  number: Int
)

object Card {
  val NULL = Card(NullColor,0)
}

//What kinds of hints may be given
sealed trait GiveHint
//What kinds of hints a player may recieve / learn
sealed trait ReceiveHint

case class HintColor(color: Color) extends ReceiveHint with GiveHint
case class HintNumber(number: Int) extends ReceiveHint with GiveHint
case object HintSameColor extends ReceiveHint
case object HintSameNumber extends ReceiveHint
case object HintSame extends ReceiveHint

type CardId = Int

trait Rules {
  def cards(): Array[Card]
  def giveHints(): Array[GiveHint]

  def handSize: Int
  def maxHints: Int
  def maxBombs: Int

  def applyHint(hint: GiveHint): ReceiveHint
  def applyHint(hint: GiveHint, card: Card): Boolean

  //Returns None if [hint] would apply to none of the cards in the hand.
  def applyHint(hint: GiveHint, hand: Array[CardId], cardMap: CardMap): Option[(ReceiveHint,Array[Boolean])]

  def isConsistent(hint: ReceiveHint, applied: Boolean, card: Card): Boolean
}

//Arrays should NOT be modified by users
class CardMap private (
  val cards: Array[Card],
  val unknown: Array[Card],
  var numUnknown: Int
) {


  //Maps CardId -> Card, or Card.NULL if not known
  val cards: Array[Card]
  //Array of unknown cards. Values are only valid in the range [0,numUnknown)
  val unknown: Array[Card]
  def numUnknown: Int
}


//Arrays should NOT be modified by users
class CardMap private (
  val cards: Array[Card],
  val unknown: Array[Card],
  var numUnknown: Int
) {


  //Maps CardId -> Card, or Card.NULL if not known
  val cards: Array[Card]
  //Array of unknown cards. Values are only valid in the range [0,numUnknown)
  val unknown: Array[Card]
  def numUnknown: Int
}
