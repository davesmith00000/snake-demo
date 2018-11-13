package snake.scenes

import indigo._
import indigoexts.lenses.Lens
import indigoexts.scenes._
import snake.model.{ControlScheme, SnakeGameModel, SnakeViewModel}
import snake.init.{GameAssets, Settings}

object ControlsScene extends Scene[SnakeGameModel, SnakeViewModel, ControlScheme, Unit] {
  val name: SceneName = SceneName("controls")

  val sceneModelLens: Lens[SnakeGameModel, ControlScheme] =
    SnakeGameModel.Lenses.controlSchemeAccessors

  val sceneViewModelLens: Lens[SnakeViewModel, Unit] =
    Lens.fixed(())

  def updateSceneModel(gameTime: GameTime, controlScheme: ControlScheme): GlobalEvent => UpdatedModel[ControlScheme] = {
    case KeyboardEvent.KeyUp(Keys.SPACE) =>
      UpdatedModel(controlScheme)
        .addGlobalEvents(SceneEvent.JumpTo(GameScene.name))

    case KeyboardEvent.KeyUp(Keys.UP_ARROW) | KeyboardEvent.KeyUp(Keys.DOWN_ARROW) =>
      UpdatedModel(controlScheme.swap)

    case _ =>
      controlScheme
  }

  def updateSceneViewModel(
      gameTime: GameTime,
      controlScheme: ControlScheme,
      sceneViewModel: Unit,
      frameInputEvents: FrameInputEvents
  ): UpdatedViewModel[Unit] =
    ()

  def updateSceneView(
      gameTime: GameTime,
      sceneModel: ControlScheme,
      sceneViewModel: Unit,
      frameInputEvents: FrameInputEvents
  ): SceneUpdateFragment = {
    val horizontalCenter: Int = (Settings.viewportWidth / Settings.magnificationLevel) / 2
    val verticalMiddle: Int   = (Settings.viewportHeight / Settings.magnificationLevel) / 2

    SceneUpdateFragment.empty
      .addUiLayerNodes(drawControlsText(24, verticalMiddle, sceneModel))
      .addUiLayerNodes(drawSelectText(horizontalCenter))
      .addUiLayerNodes(drawHitSpaceToStart(horizontalCenter, 1000, gameTime))
  }

  def drawControlsText(center: Int, middle: Int, controlScheme: ControlScheme): List[SceneGraphNode] =
    List(
      Text("select controls", center, middle - 20, 1, GameAssets.fontKey).alignLeft
    ) ++ {
      controlScheme match {
        case ControlScheme.Turning(_, _) =>
          List(
            Text("[_] direction (all arrow keys)", center, middle - 5, 1, GameAssets.fontKey).alignLeft,
            Text("[x] turn (left and right arrows)", center, middle + 10, 1, GameAssets.fontKey).alignLeft
          )

        case ControlScheme.Directed(_, _, _, _) =>
          List(
            Text("[x] direction (all arrow keys)", center, middle - 5, 1, GameAssets.fontKey).alignLeft,
            Text("[_] turn (left and right arrows)", center, middle + 10, 1, GameAssets.fontKey).alignLeft
          )
      }
    }

  def drawSelectText(center: Int): SceneGraphNode =
    Text("Up / Down arrows to select.", center, 205, 1, GameAssets.fontKey).alignCenter

  def drawHitSpaceToStart(center: Int, blinkDelay: Int, gameTime: GameTime): List[SceneGraphNode] =
    if ((gameTime.running.toInt % (blinkDelay * 2)) <= blinkDelay) {
      List(
        Text("hit space to start", center, 220, 1, GameAssets.fontKey).alignCenter
      )
    } else {
      Nil
    }
}
