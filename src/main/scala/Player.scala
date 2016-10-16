package fireflower

trait Player {
  //Called once at the start of the game after cards are drawn
  def handleGameStart(game: Game): Unit
  //Called after each action.
  def handleSeenAction(preGame: Game, sa: SeenAction, postGame: Game): Unit

  //Called when the Player should make its move.
  def getAction(game: Game): GiveAction
}
