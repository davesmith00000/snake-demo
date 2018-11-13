package snake.model.snakemodel

import indigo._
import indigoexts.grids._

sealed trait SnakeEvent extends InFrameEvent
object SnakeEvent {
  case class ScoredPoints(amount: Int, gridPoint: GridPoint) extends SnakeEvent
}
