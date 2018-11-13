package snake

import indigo._
import indigoexts.entrypoint._
import indigoexts.scenes._
import snake.model.{ControlScheme, SnakeGameModel, SnakeViewModel}
import snake.init.{GameAssets, Settings, SnakeStartupData}
import snake.scenes.{ControlsScene, GameOverScene, GameScene, StartScene}

object SnakeGame extends IndigoGameWithScenes[SnakeStartupData, SnakeGameModel, SnakeViewModel] {

  val config: GameConfig = GameConfig(
    viewport = GameViewport(Settings.viewportWidth, Settings.viewportHeight),
    frameRate = 30,
    clearColor = ClearColor.Black,
    magnification = Settings.magnificationLevel
  )

  val animations: Set[Animations] = Set()

  val assets: Set[AssetType] = GameAssets.assets

  val fonts: Set[FontInfo] = Set(GameAssets.fontInfo)

  val initialScene: Option[SceneName] = StartScene.name

  val scenes: ScenesList[SnakeGameModel, SnakeViewModel, _, _] =
    StartScene :: ControlsScene :: GameScene :: GameOverScene :: ScenesNil[SnakeGameModel, SnakeViewModel]()

  def initialModel(startupData: SnakeStartupData): SnakeGameModel =
    SnakeGameModel.initialModel(startupData, ControlScheme.directedKeys)

  def initialViewModel(startupData: SnakeStartupData): SnakeGameModel => SnakeViewModel =
    m => SnakeViewModel.initialViewModel(startupData, m)

  def setup(assetCollection: AssetCollection): Either[StartupErrors, SnakeStartupData] =
    SnakeStartupData.initialise(config.viewport, Settings.gridSize)

}
