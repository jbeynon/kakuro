package jbeynon

package object kakuro {
  case class Position(x: Int, y: Int) {
    def index(size: Int) = x + y * size
  }

  case class Board(size: Int, values: Vector[Int])

  case class Game(board: Board, pens: Vector[Int], pencils: Vector[Set[Int]]) {
    def isComplete = pens == board.values
  }

  def getGrid[T](grid: Vector[T], board: Board, position: Position) =
    grid(position.index(board.size))

  def updateGrid[T](grid: Vector[T], board: Board, position: Position, f: T => T) =
    grid.updated(position.index(board.size), f(getGrid(grid, board, position)))

  def createClues(board: Board) = {
    val (acrosses, downs) = (for {
      x <- 0 until board.size
      y <- 0 until board.size
      value = getGrid(board.values, board, Position(x, y))
      if (value == 0)
    } yield {
      val across =
        if (x < board.size - 1 && getGrid(board.values, board, Position(x + 1, y)) > 0)
          Some((Position(x, y), (for { i <- x + 1 until board.size } yield { getGrid(board.values, board, Position(i, y)) }).takeWhile(_ > 0).sum))
        else None
      val down =
        if (y < board.size - 1 && getGrid(board.values, board, Position(x, y + 1)) > 0)
          Some((Position(x, y), (for { i <- y + 1 until board.size } yield { getGrid(board.values, board, Position(x, i)) }).takeWhile(_ > 0).sum))
        else None

      (across, down)
    }).unzip

    (acrosses.flatten.toMap, downs.flatten.toMap)
  }

  val sampleGame = {
    val values = Vector(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 2, 7, 0, 0, 1, 7, 3, 0, 0, 8, 4, 0, 0,
      0, 8, 9, 0, 0, 2, 4, 1, 0, 8, 9, 7, 0, 0,
      0, 0, 3, 1, 2, 8, 0, 2, 1, 7, 0, 0, 4, 7,
      0, 0, 0, 2, 9, 0, 0, 0, 2, 1, 0, 0, 8, 9,
      0, 1, 3, 0, 0, 3, 1, 0, 0, 9, 7, 8, 5, 0,
      0, 2, 1, 0, 7, 8, 9, 5, 0, 0, 8, 9, 0, 0,
      0, 0, 8, 3, 6, 0, 7, 9, 6, 0, 2, 7, 4, 0,
      0, 0, 0, 2, 8, 0, 0, 2, 1, 3, 4, 0, 1, 2,
      0, 0, 4, 1, 3, 2, 0, 0, 8, 7, 0, 0, 8, 9,
      0, 7, 1, 0, 0, 7, 2, 0, 0, 0, 2, 9, 0, 0,
      0, 9, 3, 0, 0, 3, 1, 7, 0, 3, 1, 5, 2, 0,
      0, 0, 0, 8, 7, 9, 0, 5, 8, 9, 0, 0, 3, 1,
      0, 0, 0, 9, 1, 0, 0, 9, 7, 8, 0, 0, 8, 9)

    Game(Board(14, values), Vector.fill(14 * 14)(0), Vector.fill(14 * 14)(Set.empty))
  }
}
