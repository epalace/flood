package flood

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class Board(val matrix: Array[Array[Int]]) {
  val size = matrix.length

  def newCopy: Board = {
    val newMatrix = Array.ofDim[Int](size, size)
    for { x <- 0 until size } { Array.copy(matrix(x), 0, newMatrix(x), 0, size) }
    new Board(newMatrix)
  }

  def flood(newColor: Int): (Board, Int) = {
    val dest = newCopy
    val size = dest.size

    val colorToChange = matrix(0)(0)
    val visited = mutable.Set.empty[Point]

    def recFlood(p: Point): Unit = {
      visited += p
      dest.matrix(p.x)(p.y) = newColor
      for  {
        x <- p.x - 1 to p.x + 1 if x >= 0 && x < size
        y <- p.y - 1 to p.y + 1 if y >= 0 && y < size
        if x == p.x && y != p.y || x != p.x && y == p.y
        if matrix(x)(y) == colorToChange
      } {
        val newPoint = Point(x, y)
        if (!visited.contains(newPoint)) {
          recFlood(newPoint)
        }
      }
    }

    recFlood(Point(0, 0))
    (dest, visited.size)
  }

  override def equals(obj: Any): Boolean = {
    val that = obj.asInstanceOf[Board]
    size == that.size && (0 until size).forall(i => java.util.Arrays.equals(matrix(i), that.matrix(i)))
  }

  override def hashCode(): Int = java.util.Arrays.deepHashCode(matrix.asInstanceOf[Array[AnyRef]])

  override def toString(): String = matrix.map(_.mkString(", ")).mkString("\n")

}

case class Point(x: Int, y: Int)

object Board {

  def fromString(str: String): Board = {
    val board = Source.fromString(str).getLines.filterNot(_.trim.isEmpty).map { line =>
      line.split(",").map(_.trim.toInt)
    }.toArray
    val numRows = board.length
    for { rowIndex <- 0 until numRows} {
      val numColumns = board(rowIndex).length
      if (numColumns != numRows) {
        throw new IllegalArgumentException(s"Row #rowIndex illegal num columns ${numColumns}. Expected: ${numRows}")
      }
    }
    new Board(board)
  }

  def random(size: Int, numColors: Int) = {
    val board = empty(size)
    for { x <- 0 to size; y <- 0 to size } { board.matrix(x)(y) = Random.nextInt(numColors) }
    board
  }

  def empty(size: Int): Board = new Board(Array.ofDim[Int](size, size))
}
