package snake.scenes
import indigo.gameengine.GameTime
import indigo.gameengine.events.FrameTick
import indigoexts.grid.{GridPoint, GridSize}
import org.scalatest.{FunSpec, Matchers}
import snake.gamelogic.ModelLogic
import snake.model.{ControlScheme, GameModel}

class ModelLogicSpec extends FunSpec with Matchers {

  val defaultDelay: Int = 100
  val maxTime: Int      = 1000
  val lowerBound: Int   = 10

  val model: GameModel =
    ModelLogic.initialModel(
      GridSize.apply(5, 5, 10),
      ControlScheme.directedKeys
    )

  describe("basic model updates") {

    it("should advance the game on frame tick") {
      val actual = ModelLogic.update(GameTime.is(150, 150, 33), model)(FrameTick).model
      val expected = model.copy(
        snake = model.snake.copy(start = GridPoint(2, 2)),
        gameState = model.gameState.updateNow(150, model.gameState.lastSnakeDirection)
      )

      actual shouldEqual expected
    }

  }

  describe("Shrinking on dead") {

    it("should be able to calculate a reasonable tick delay based on snake length") {

      withClue("Normal delay") {
        ModelLogic.calculateShrinkDelay(defaultDelay, 1, maxTime, lowerBound) shouldEqual defaultDelay
        ModelLogic.calculateShrinkDelay(defaultDelay, 5, maxTime, lowerBound) shouldEqual defaultDelay
        ModelLogic.calculateShrinkDelay(defaultDelay, 10, maxTime, lowerBound) shouldEqual defaultDelay
      }

      withClue("Proportional delay") {
        ModelLogic.calculateShrinkDelay(defaultDelay, 50, maxTime, lowerBound) shouldEqual (maxTime / 50)
        ModelLogic.calculateShrinkDelay(defaultDelay, 100, maxTime, lowerBound) shouldEqual (maxTime / 100)
        ModelLogic.calculateShrinkDelay(defaultDelay, 20000, maxTime, lowerBound) shouldEqual lowerBound
      }

    }

  }

}
