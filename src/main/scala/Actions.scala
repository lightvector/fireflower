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

//Not actually a legal hint, but used in when we need a hint type but don't exactly know what it could be. Applies to no cards.
case object UnknownHint extends SeenHintType with GiveHintType


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

object GiveAction {
  def ofString(s:String) = {
    def fail() = throw new Exception()
    try {
      val pieces = s.split("\\s+").filter(_.nonEmpty).toArray

      if(pieces.length < 1) fail()
      pieces(0).toLowerCase match {
        case ("discard" | "givediscard") =>
          if(pieces.length < 2) fail()
          val hid = pieces(1).toInt-1
          if(hid < 0) fail()
          GiveDiscard(hid)
        case ("play" | "giveplay" | "bomb") =>
          if(pieces.length < 2) fail()
          val hid = pieces(1).toInt-1
          if(hid < 0) fail()
          GivePlay(hid)
        case ("hint" | "givehint" | "clue") =>
          if(pieces.length < 3) fail()
          val pid = pieces(1).toInt
          if(pid < 0) fail()
          val hint = pieces(2).toLowerCase match {
            case ("r" | "red") => HintColor(Red)
            case ("g" | "green") => HintColor(Green)
            case ("y" | "yellow") => HintColor(Yellow)
            case ("b" | "blue") => HintColor(Blue)
            case ("w" | "white") => HintColor(White)
            case ("m" | "multi" | "multicolor" | "rainbow") => HintColor(MultiColor)
            case ("nullcolor") => HintColor(NullColor)
            case ("1") => HintNumber(0)
            case ("2") => HintNumber(1)
            case ("3") => HintNumber(2)
            case ("4") => HintNumber(3)
            case ("5") => HintNumber(4)
            case ("u" | "?" | "unknown" | "unknownhint") => UnknownHint
          }
          GiveHint(pid,hint)
      }
    }
    catch {
      case e:Exception => throw new Exception("Could not parse GiveAction: " + s + "\n", e)
    }
  }
}
