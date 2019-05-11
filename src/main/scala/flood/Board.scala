package flood

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class Board(val matrix: Array[Array[Int]]) {
  val size = matrix.length
  def numColors = matrix.map(_.max).max + 1

  def newCopy: Board = {
    val newMatrix = Array.ofDim[Int](size, size)
    for { x <- 0 until size } { Array.copy(matrix(x), 0, newMatrix(x), 0, size) }
    new Board(newMatrix)
  }

  def floodCount: Int = {
    val visited = mutable.Set.empty[Point]
    recFlood(Point(0, 0), matrix(0)(0), visited, _ => ())
    visited.size
  }

  def flood(newColor: Int): Board = {
    val dest = newCopy

    val colorToChange = matrix(0)(0)
    val visited = mutable.Set.empty[Point]

    def action(p: Point): Unit = dest.matrix(p.x)(p.y) = newColor

    recFlood(Point(0, 0), colorToChange, visited, action)
    dest
  }

  private[flood] def recFlood(p: Point, sourceColor: Int, visited: mutable.Set[Point], visitor: Point => Unit): Unit = {
    def visit(x: Int, y: Int) = {
      if (x >= 0 && x < size && y >= 0 && y < size && matrix(x)(y) == sourceColor){
        val newPoint = Point(x,y)
        if (!visited.contains(newPoint)) {
          recFlood(newPoint, sourceColor, visited, visitor)
        }
      }
    }

    visited += p
    visitor(p)

    visit(p.x + 1, p.y)
    visit(p.x, p.y + 1)
    visit(p.x - 1, p.y)
    visit(p.x, p.y - 1)
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

  def parse(str: String): Board = {
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
    for { x <- 0 until size; y <- 0 until size } { board.matrix(x)(y) = Random.nextInt(numColors) }
    board
  }

  def empty(size: Int): Board = new Board(Array.ofDim[Int](size, size))
}
