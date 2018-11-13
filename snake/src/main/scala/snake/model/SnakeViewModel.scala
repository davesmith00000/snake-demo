package snake.model

import indigo._
import snake.gamelogic.ViewLogic
import snake.init.{SnakeStartupData, StaticAssets}

case class SnakeViewModel(walls: Group, points: List[PointsScored], staticAssets: StaticAssets) {
  def reset: SnakeViewModel =
    if (points.nonEmpty) this.copy(points = Nil) else this
}
object SnakeViewModel {
  def initialViewModel(startupData: SnakeStartupData, snakeModel: SnakeGameModel): SnakeViewModel =
    SnakeViewModel(
      walls = Group(
        snakeModel.gameModel.gameMap.findWalls.map { wall =>
          startupData.staticAssets.wall
            .moveTo(ViewLogic.gridPointToPoint(wall.gridPoint, startupData.gridSize))
        }
      ),
      points = Nil,
      staticAssets = startupData.staticAssets
    )
}

case class PointsScored(text: String, position: Point, ttl: Int) {
  def update(delta: Int): PointsScored =
    this.copy(
      ttl = if (ttl - delta > 0) ttl - delta else 0,
      position = position + Point(0, -1)
    )
}
