package snake.model.snakemodel

import indigoexts.grids._

case class Snake(start: GridPoint, body: List[GridPoint], direction: SnakeDirection, status: SnakeStatus) {

  def goUp: Snake =
    Snake.goUp(this)

  def goDown: Snake =
    Snake.goDown(this)

  def goLeft: Snake =
    Snake.goLeft(this)

  def goRight: Snake =
    Snake.goRight(this)

  def turnLeft: Snake =
    Snake.turnLeft(this)

  def turnRight: Snake =
    Snake.turnRight(this)

  def update(gridSize: GridSize, collisionCheck: GridPoint => CollisionCheckOutcome): (Snake, CollisionCheckOutcome) =
    Snake.update(this, gridSize, collisionCheck)

  def end: GridPoint =
    Snake.end(this)

  def grow: Snake =
    Snake.grow(this)

  def shrink: Snake =
    Snake.shrink(this)

  def crash: Snake =
    Snake.crash(this)

  def length: Int =
    1 + body.length

  def givePath: List[GridPoint] =
    start :: body

  def givePathList: List[(Int, Int)] =
    (start :: body).map(p => (p.x, p.y))

}
object Snake {

  def apply(start: GridPoint): Snake =
    Snake(start, Nil, SnakeDirection.Up, SnakeStatus.Alive)

  def apply(x: Int, y: Int): Snake =
    Snake(GridPoint(x, y), Nil, SnakeDirection.Up, SnakeStatus.Alive)

  def turnLeft(snake: Snake): Snake =
    snake.copy(direction = snake.direction.turnLeft)

  def turnRight(snake: Snake): Snake =
    snake.copy(direction = snake.direction.turnRight)

  def goUp(snake: Snake): Snake =
    snake.copy(direction = snake.direction.goUp)

  def goDown(snake: Snake): Snake =
    snake.copy(direction = snake.direction.goDown)

  def goLeft(snake: Snake): Snake =
    snake.copy(direction = snake.direction.goLeft)

  def goRight(snake: Snake): Snake =
    snake.copy(direction = snake.direction.goRight)

  def end(snake: Snake): GridPoint =
    snake.body.reverse.headOption.getOrElse(snake.start)

  def grow(snake: Snake): Snake =
    snake.copy(body = snake.body :+ end(snake))

  def shrink(snake: Snake): Snake =
    snake.copy(body = snake.body.dropRight(1))

  def crash(snake: Snake): Snake =
    snake.copy(status = SnakeStatus.Dead)

  def update(
      snake: Snake,
      gridSize: GridSize,
      collisionCheck: GridPoint => CollisionCheckOutcome
  ): (Snake, CollisionCheckOutcome) =
    (nextPosition(gridSize) andThen collisionCheck andThen snakeUpdate(snake))(snake)

  def nextPosition(gridSize: GridSize): Snake => GridPoint =
    snake =>
      snake.direction
        .oneSquareForward(snake.start)
        .wrap(gridSize)

  def snakeUpdate(snake: Snake): CollisionCheckOutcome => (Snake, CollisionCheckOutcome) = {
    case oc @ CollisionCheckOutcome.NoCollision(pt) =>
      (moveToPosition(snake, pt), oc)

    case oc @ CollisionCheckOutcome.PickUp(pt) =>
      (moveToPosition(snake.grow, pt), oc)

    case oc @ CollisionCheckOutcome.Crashed(_) =>
      (snake.crash, oc)
  }

  def moveToPosition(snake: Snake, snakePoint: GridPoint): Snake =
    snake match {
      case Snake(_, Nil, d, s) =>
        Snake(snakePoint, Nil, d, s)

      case Snake(h, l, d, s) =>
        Snake(snakePoint, h :: l.reverse.drop(1).reverse, d, s)
    }

}

sealed trait TurnDirection
object TurnDirection {
  case object Left  extends TurnDirection
  case object Right extends TurnDirection
}

sealed trait SnakeDirection {

  def makeLegalTurn(snake: Snake): Option[Snake] =
    (this, snake.direction) match {
      case (SnakeDirection.Up, SnakeDirection.Up)       => Some(snake)
      case (SnakeDirection.Up, SnakeDirection.Left)     => Some(snake)
      case (SnakeDirection.Up, SnakeDirection.Right)    => Some(snake)
      case (SnakeDirection.Down, SnakeDirection.Down)   => Some(snake)
      case (SnakeDirection.Down, SnakeDirection.Left)   => Some(snake)
      case (SnakeDirection.Down, SnakeDirection.Right)  => Some(snake)
      case (SnakeDirection.Left, SnakeDirection.Left)   => Some(snake)
      case (SnakeDirection.Left, SnakeDirection.Up)     => Some(snake)
      case (SnakeDirection.Left, SnakeDirection.Down)   => Some(snake)
      case (SnakeDirection.Right, SnakeDirection.Right) => Some(snake)
      case (SnakeDirection.Right, SnakeDirection.Up)    => Some(snake)
      case (SnakeDirection.Right, SnakeDirection.Down)  => Some(snake)
      case _                                            => None
    }

  def turnLeft: SnakeDirection =
    SnakeDirection.turn(this, TurnDirection.Left)

  def turnRight: SnakeDirection =
    SnakeDirection.turn(this, TurnDirection.Right)

  def goUp: SnakeDirection =
    SnakeDirection.go(this, SnakeDirection.Up)

  def goDown: SnakeDirection =
    SnakeDirection.go(this, SnakeDirection.Down)

  def goLeft: SnakeDirection =
    SnakeDirection.go(this, SnakeDirection.Left)

  def goRight: SnakeDirection =
    SnakeDirection.go(this, SnakeDirection.Right)

  def oneSquareForward(current: GridPoint): GridPoint =
    SnakeDirection.oneSquareForward(this, current)

}
object SnakeDirection {

  case object Up    extends SnakeDirection
  case object Down  extends SnakeDirection
  case object Left  extends SnakeDirection
  case object Right extends SnakeDirection

  def go(snakeDirection: SnakeDirection, goDirection: SnakeDirection): SnakeDirection =
    (snakeDirection, goDirection) match {
      case (Up, Left) =>
        Left

      case (Up, Right) =>
        Right

      case (Down, Left) =>
        Left

      case (Down, Right) =>
        Right

      case (Left, Up) =>
        Up

      case (Left, Down) =>
        Down

      case (Right, Up) =>
        Up

      case (Right, Down) =>
        Down

      case (current, _) =>
        current
    }

  def turn(snakeDirection: SnakeDirection, turnDirection: TurnDirection): SnakeDirection =
    (snakeDirection, turnDirection) match {
      case (Up, TurnDirection.Left) =>
        Left

      case (Up, TurnDirection.Right) =>
        Right

      case (Down, TurnDirection.Left) =>
        Right

      case (Down, TurnDirection.Right) =>
        Left

      case (Left, TurnDirection.Left) =>
        Down

      case (Left, TurnDirection.Right) =>
        Up

      case (Right, TurnDirection.Left) =>
        Up

      case (Right, TurnDirection.Right) =>
        Down
    }

  def oneSquareForward(snakeDirection: SnakeDirection, current: GridPoint): GridPoint =
    snakeDirection match {
      case Up =>
        current + GridPoint.Up

      case Down =>
        current + GridPoint.Down

      case Left =>
        current + GridPoint.Left

      case Right =>
        current + GridPoint.Right
    }

}

sealed trait CollisionCheckOutcome {
  val gridPoint: GridPoint
}
object CollisionCheckOutcome {
  case class NoCollision(gridPoint: GridPoint) extends CollisionCheckOutcome
  case class PickUp(gridPoint: GridPoint)      extends CollisionCheckOutcome
  case class Crashed(gridPoint: GridPoint)     extends CollisionCheckOutcome
}

sealed trait SnakeStatus {
  def isDead: Boolean =
    this match {
      case SnakeStatus.Alive => false
      case SnakeStatus.Dead  => true
    }
}
object SnakeStatus {
  case object Alive extends SnakeStatus
  case object Dead  extends SnakeStatus
}
