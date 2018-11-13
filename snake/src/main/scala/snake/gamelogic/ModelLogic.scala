package snake.gamelogic

import indigo._
import indigoexts.grids._
import indigoexts.scenemanager._
import snake.init.GameAssets
import snake.model._
import snake.model.arena.MapElement.Apple
import snake.model.arena.{Arena, GameMap, MapElement}
import snake.model.snakemodel.{CollisionCheckOutcome, Snake, SnakeEvent}
import snake.scenes.GameOverScene

object ModelLogic {

  val ScoreIncrement: Int = 100

  def initialModel(gridSize: GridSize, controlScheme: ControlScheme): GameModel =
    GameModel(
      gridSize = gridSize,
      snake = Snake(
        gridSize.centre.x,
        gridSize.centre.y - (gridSize.centre.y / 2)
      ).grow.grow,
      gameState = GameState.Running.start,
      gameMap = Arena.genLevel(gridSize),
      score = 0,
      tickDelay = 100,
      controlScheme = controlScheme
    )

  def hitTest(
      gameMap: GameMap,
      body: List[GridPoint],
      lastPosition: GridPoint,
      useNextPosition: Boolean
  ): GridPoint => CollisionCheckOutcome = nextPosition => {
    val pt: GridPoint = if (useNextPosition) nextPosition else lastPosition

    if (useNextPosition && body.contains(pt)) CollisionCheckOutcome.Crashed(pt)
    else
      gameMap.fetchElementAt(pt.x, pt.y) match {
        case Some(MapElement.Apple(_)) =>
          CollisionCheckOutcome.PickUp(pt)

        case Some(MapElement.Wall(_)) =>
          CollisionCheckOutcome.Crashed(pt)

        case None =>
          CollisionCheckOutcome.NoCollision(pt)
      }
  }

  def update(gameTime: GameTime, state: GameModel): GlobalEvent => UpdatedModel[GameModel] =
    gameEvent =>
      state.gameState match {
        case s @ GameState.Running(_, _) =>
          updateRunning(gameTime, state, s)(gameEvent)

        case s @ GameState.Crashed(_, _, _, _) =>
          updateCrashed(gameTime, state, s)(gameEvent)
    }

  def updateRunning(gameTime: GameTime,
                    state: GameModel,
                    runningDetails: GameState.Running): GlobalEvent => UpdatedModel[GameModel] = {
    case FrameTick =>
      updateBasedOnCollision(gameTime) {
        state.update(
          gameTime,
          state.gameMap.gridSize,
          hitTest(
            state.gameMap,
            state.snake.givePath,
            state.snake.start,
            useNextPosition = (gameTime.running - runningDetails.lastUpdated) > state.tickDelay
          )
        )
      }

    case e: KeyboardEvent =>
      state.copy(
        snake = state.controlScheme.instructSnake(e, state.snake, runningDetails.lastSnakeDirection)
      )

    case _ =>
      state
  }

  def updateBasedOnCollision(gameTime: GameTime): ((GameModel, CollisionCheckOutcome)) => UpdatedModel[GameModel] = {
    case (gameModel, CollisionCheckOutcome.Crashed(_)) =>
      UpdatedModel(
        gameModel.copy(
          gameState = gameModel.gameState match {
            case c @ GameState.Crashed(_, _, _, _) =>
              c

            case r @ GameState.Running(_, _) =>
              r.crash(gameTime.running, gameModel.snake.length)
          }
        )
      ).addGlobalEvents(PlaySound(GameAssets.soundLose, Volume.Max))

    case (gameModel, CollisionCheckOutcome.PickUp(pt)) =>
      UpdatedModel(
        gameModel.copy(
          snake = gameModel.snake.grow,
          gameMap = gameModel.gameMap
            .removeElement(pt)
            .insertElement(
              Apple(
                gameModel.gameMap
                  .findEmptySpace(pt :: gameModel.snake.givePath)
              )
            ),
          score = gameModel.score + ScoreIncrement
        )
      ).addInFrameEvents(SnakeEvent.ScoredPoints(ScoreIncrement, pt))
        .addGlobalEvents(PlaySound(GameAssets.soundPoint, Volume.Max))

    case (gameModel, CollisionCheckOutcome.NoCollision(_)) =>
      gameModel
  }

  def updateCrashed(gameTime: GameTime,
                    state: GameModel,
                    crashDetails: GameState.Crashed): GlobalEvent => UpdatedModel[GameModel] = {
    case FrameTick if gameTime.running <= crashDetails.crashedAt + 750 =>
      //Pause briefly on collision
      state

    case FrameTick if state.snake.length > 1 =>
      val delay = calculateShrinkDelayWithDefaults(state.tickDelay, crashDetails.snakeLengthOnCrash)

      if (gameTime.running >= state.gameState.lastUpdated + delay) {
        state.copy(
          snake = state.snake.shrink,
          gameState = state.gameState.updateNow(gameTime.running, state.gameState.lastSnakeDirection)
        )
      } else state

    case FrameTick if state.snake.length == 1 =>
      UpdatedModel(state)
        .addGlobalEvents(SceneEvent.JumpTo(GameOverScene.name))

    case _ =>
      state
  }

  def calculateShrinkDelayWithDefaults(tickDelay: Int, snakeLength: Int): Int =
    calculateShrinkDelay(tickDelay, snakeLength, 2000, 10)

  def calculateShrinkDelay(tickDelay: Int, snakeLength: Int, totalTime: Int, lowerBound: Int): Int =
    Math.min(tickDelay, Math.max(lowerBound, totalTime / snakeLength))

}
