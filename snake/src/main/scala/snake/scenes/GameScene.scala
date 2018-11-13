package snake.scenes

import indigo._
import indigoexts.lenses._
import indigoexts.scenemanager._
import snake.gamelogic.{ModelLogic, ViewLogic}
import snake.model.{GameModel, SnakeGameModel, SnakeViewModel}

object GameScene extends Scene[SnakeGameModel, SnakeViewModel, GameModel, SnakeViewModel] {
  val name: SceneName = SceneName("game scene")

  val sceneModelLens: Lens[SnakeGameModel, GameModel] =
    SnakeGameModel.Lenses.gameLens

  val sceneViewModelLens: Lens[SnakeViewModel, SnakeViewModel] =
    Lens.keepLatest

  def updateSceneModel(gameTime: GameTime, gameModel: GameModel): GlobalEvent => UpdatedModel[GameModel] =
    ModelLogic.update(gameTime, gameModel)

  def updateSceneViewModel(
      gameTime: GameTime,
      gameModel: GameModel,
      snakeViewModel: SnakeViewModel,
      frameInputEvents: FrameInputEvents
  ): UpdatedViewModel[SnakeViewModel] =
    ViewLogic.updateViewModel(gameTime, gameModel, snakeViewModel, frameInputEvents)

  def updateSceneView(
      gameTime: GameTime,
      gameModel: GameModel,
      snakeViewModel: SnakeViewModel,
      frameInputEvents: FrameInputEvents
  ): SceneUpdateFragment =
    ViewLogic.update(gameModel, snakeViewModel)
}
