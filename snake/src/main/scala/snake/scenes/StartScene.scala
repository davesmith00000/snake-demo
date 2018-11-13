package snake.scenes

import indigo._
import indigoexts.lenses.Lens
import indigoexts.scenes._
import snake.init.{GameAssets, Settings}
import snake.model.{SnakeGameModel, SnakeViewModel}

object StartScene extends Scene[SnakeGameModel, SnakeViewModel, SnakeGameModel, SnakeViewModel] {
  val name: SceneName = SceneName("start")

  val sceneModelLens: Lens[SnakeGameModel, SnakeGameModel] =
    Lens.keepLatest

  val sceneViewModelLens: Lens[SnakeViewModel, SnakeViewModel] =
    Lens.keepLatest

  def updateSceneModel(gameTime: GameTime, snakeGameModel: SnakeGameModel): GlobalEvent => UpdatedModel[SnakeGameModel] = {
    case KeyboardEvent.KeyUp(Keys.SPACE) =>
      UpdatedModel(snakeGameModel.reset)
        .addGlobalEvents(SceneEvent.JumpTo(ControlsScene.name))

    case _ =>
      snakeGameModel
  }

  def updateSceneViewModel(
      gameTime: GameTime,
      snakeGameModel: SnakeGameModel,
      snakeViewModel: SnakeViewModel,
      frameInputEvents: FrameInputEvents
  ): UpdatedViewModel[SnakeViewModel] =
    snakeViewModel.reset

  def updateSceneView(
      gameTime: GameTime,
      snakeGameModel: SnakeGameModel,
      snakeViewModel: SnakeViewModel,
      frameInputEvents: FrameInputEvents
  ): SceneUpdateFragment = {
    val horizontalCenter: Int = (Settings.viewportWidth / Settings.magnificationLevel) / 2
    val verticalMiddle: Int   = (Settings.viewportHeight / Settings.magnificationLevel) / 2

    SceneUpdateFragment.empty
      .addUiLayerNodes(drawTitleText(horizontalCenter, verticalMiddle))
      .addUiLayerNodes(drawHitSpaceToStart(horizontalCenter, 1000, gameTime))
      .withAudio(
        SceneAudio(
          SceneAudioSource(
            BindingKey("intro music"),
            PlaybackPattern.SingleTrackLoop(
              Track(GameAssets.soundIntro)
            )
          )
        )
      )
  }

  def drawTitleText(center: Int, middle: Int): List[SceneGraphNode] =
    List(
      Text("snake!", center, middle - 20, 1, GameAssets.fontKey).alignCenter,
      Text("presented in glorious 1 bit graphics", center, middle - 5, 1, GameAssets.fontKey).alignCenter,
      Text("Made by Dave", center, middle + 10, 1, GameAssets.fontKey).alignCenter
    )

  def drawHitSpaceToStart(center: Int, blinkDelay: Int, gameTime: GameTime): List[SceneGraphNode] =
    if ((gameTime.running.toInt % (blinkDelay * 2)) <= blinkDelay) {
      List(
        Text("hit space to start", center, 220, 1, GameAssets.fontKey).alignCenter
      )
    } else {
      Nil
    }
}
