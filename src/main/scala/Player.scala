package fireflower

abstract class Player {
  //Called once at the start of the game after cards are drawn
  def handleGameStart(game: Game): Unit
  //Called after each action.
  def handleSeenAction(sa: SeenAction, postGame: Game): Unit

  //Called when the Player should make its move.
  def getAction(game: Game): GiveAction
}
