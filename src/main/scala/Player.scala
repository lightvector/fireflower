/**
  * Player.scala
  * Hanabi player implementations must satisfy this interface.
  */

package fireflower

abstract class Player {
  //Called once at the start of the game after cards are drawn
  def handleGameStart(game: Game): Unit
  //Called after each action.
  def handleSeenAction(sa: SeenAction, postGame: Game): Unit

  //Called when the Player should make its move.
  def getAction(game: Game): GiveAction
}

//A PlayerGen is an object that specifies the players for a given game, with a seed
//to randomize the players in case they are players that could behave randomly.
trait PlayerGen {
  def genPlayers(rules: Rules, seed: Long): Array[Player]
}
