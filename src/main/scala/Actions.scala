/**
  * Actions.scala
  * Basic types for hints and other actions
  */
package fireflower

//What kinds of hints may be given
sealed trait GiveHintType
//What kinds of hints a player may recieve / observe / learn
sealed trait SeenHintType

case class HintColor(color: Color) extends SeenHintType with GiveHintType
case class HintNumber(number: Int) extends SeenHintType with GiveHintType
case object HintSameColor extends SeenHintType
case object HintSameNumber extends SeenHintType
case object HintSame extends SeenHintType

//What kinds of actions a player may take
sealed trait GiveAction
case class GiveDiscard(hid: HandId) extends GiveAction
case class GivePlay(hid: HandId) extends GiveAction
case class GiveHint(pid: PlayerId, hint: GiveHintType) extends GiveAction

//What kinds of actions a player may observe
sealed trait SeenAction
case class SeenDiscard(hid: HandId, cid: CardId) extends SeenAction
case class SeenPlay(hid: HandId, cid: CardId) extends SeenAction
case class SeenBomb(hid: HandId, cid: CardId) extends SeenAction
case class SeenHint(pid: PlayerId, hint: SeenHintType, appliedTo: Array[Boolean]) extends SeenAction
