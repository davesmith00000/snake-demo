package snake.model

import indigo.GameTime
import indigoexts.grids.{GridPoint, GridSize}
import snake.model.arena.GameMap
import snake.model.snakemodel.{CollisionCheckOutcome, Snake}

case class GameModel(gridSize: GridSize,
                     snake: Snake,
                     gameState: GameState,
                     gameMap: GameMap,
                     score: Int,
                     tickDelay: Int,
                     controlScheme: ControlScheme) {

  def update(
      gameTime: GameTime,
      gridSize: GridSize,
      collisionCheck: GridPoint => CollisionCheckOutcome
  ): (GameModel, CollisionCheckOutcome) =
    snake.update(gridSize, collisionCheck) match {
      case (s, outcome) if gameTime.running >= gameState.lastUpdated + tickDelay =>
        (this.copy(snake = s, gameState = gameState.updateNow(gameTime.running, snake.direction)), outcome)

      case (_, outcome) =>
        (this, outcome)
    }
}
