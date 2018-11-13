package snake.model

import snake.model.snakemodel.SnakeDirection

sealed trait GameState {
  val hasCrashed: Boolean
  val lastUpdated: Double
  val lastSnakeDirection: SnakeDirection
  def updateNow(time: Double, currentDirection: SnakeDirection): GameState
}
object GameState {
  case class Crashed(crashedAt: Double, snakeLengthOnCrash: Int, lastUpdated: Double, lastSnakeDirection: SnakeDirection)
      extends GameState {
    val hasCrashed: Boolean = true

    def updateNow(time: Double, currentDirection: SnakeDirection): GameState.Crashed =
      this.copy(lastUpdated = time, lastSnakeDirection = currentDirection)
  }
  case class Running(lastUpdated: Double, lastSnakeDirection: SnakeDirection) extends GameState {
    val hasCrashed: Boolean = false

    def updateNow(time: Double, currentDirection: SnakeDirection): GameState.Running =
      this.copy(lastUpdated = time, lastSnakeDirection = currentDirection)

    def crash(crashedAt: Double, snakeLengthOnCrash: Int): GameState.Crashed =
      GameState.Crashed(crashedAt, snakeLengthOnCrash: Int, lastUpdated, lastSnakeDirection)
  }
  object Running {
    val start: Running = GameState.Running(0, SnakeDirection.Up)
  }
}
