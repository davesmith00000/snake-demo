package snake.gamelogic

import indigo._
import indigoexts.grids.{GridPoint, GridSize}
import snake.init.{GameAssets, Settings, StaticAssets}
import snake.model.arena.GameMap
import snake.model.snakemodel.SnakeEvent
import snake.model.{GameModel, PointsScored, SnakeViewModel}

object ViewLogic {

  def updateViewModel(
      gameTime: GameTime,
      sceneModel: GameModel,
      sceneViewModel: SnakeViewModel,
      frameInputEvents: FrameInputEvents
  ): UpdatedViewModel[SnakeViewModel] =
    sceneViewModel.copy(
      points = frameInputEvents.inFrameEvents
        .foldLeft(sceneViewModel)(addNewPointsScored(sceneModel.gridSize))
        .points
        .map(_.update(gameTime.delta.toInt))
        .filter(_.ttl > 0)
    )

  def addNewPointsScored(gridSize: GridSize): (SnakeViewModel, InFrameEvent) => SnakeViewModel = {
    case (vm, SnakeEvent.ScoredPoints(amount, pt)) =>
      vm.copy(
        points =
          vm.points :+ PointsScored(amount.toString, gridPointToPoint(pt, gridSize), 1000)
      )

    case (vm, _) =>
      vm
  }

  def gridPointToPoint(gridPoint: GridPoint, gridSize: GridSize): Point =
    Point(gridPoint.x * gridSize.gridSquareSize, ((gridSize.rows - 1) - gridPoint.y) * gridSize.gridSquareSize)

  def update(model: GameModel, snakeViewModel: SnakeViewModel): SceneUpdateFragment =
    SceneUpdateFragment.empty
      .addGameLayerNodes(
        gameLayer(
          model,
          snakeViewModel.staticAssets,
          snakeViewModel.walls,
          snakeViewModel.points
        )
      )

  def gameLayer(
      currentState: GameModel,
      staticAssets: StaticAssets,
      walls: Group,
      points: List[PointsScored]
  ): List[SceneGraphNode] =
    walls ::
      drawApple(currentState.gameMap, staticAssets) ++
      drawSnake(currentState, staticAssets.snake) ++
      drawScore(currentState.score) ++
      drawPoints(points)

  def drawApple(gameMap: GameMap, staticAssets: StaticAssets): List[Graphic] =
    gameMap.findApples.map(
      a => staticAssets.apple.moveTo(gridPointToPoint(a.gridPoint, gameMap.gridSize))
    )

  def drawSnake(currentState: GameModel, snakeAsset: Graphic): List[Graphic] =
    currentState.snake.givePath.map { pt =>
      snakeAsset.moveTo(gridPointToPoint(pt, currentState.gameMap.gridSize))
    }

  def drawScore(score: Int): List[SceneGraphNode] =
    List(
      Text(
        score.toString,
        (Settings.viewportWidth / Settings.magnificationLevel) - 3,
        (Settings.viewportHeight / Settings.magnificationLevel) - Settings.footerHeight + 21,
        1,
        GameAssets.fontKey
      ).alignRight
    )

  def drawPoints(points: List[PointsScored]): List[SceneGraphNode] =
    points.map { p =>
      Text(p.text, p.position.x, p.position.y, 2, GameAssets.fontKey)
    }

}
