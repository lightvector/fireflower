package fireflower

import RichImplicits._

sealed trait Color
case object Red extends Color
case object Yellow extends Color
case object Green extends Color
case object Blue extends Color
case object White extends Color

case class Card(
  color: Color,
  number: Int
)

sealed trait HintProperty
case class HintColor(color: Color) extends HintProperty
case class HintNumber(number: Int) extends HintProperty
