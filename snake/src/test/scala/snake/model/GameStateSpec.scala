package snake.model

import org.scalatest.{FunSpec, Matchers}
import snake.model.snakemodel.SnakeDirection

class GameStateSpec extends FunSpec with Matchers {

  describe("GameState") {

    it("should do a simple update") {

      val actual   = GameState.Running.start.updateNow(100, SnakeDirection.Up)
      val expected = GameState.Running(100, SnakeDirection.Up)

      actual shouldEqual expected

    }

  }

}
