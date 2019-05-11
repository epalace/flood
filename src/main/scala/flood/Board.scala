package flood

import java.util

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class Board(private[flood] val array: Array[Int], val size: Int) {
  def numColors = array.max + 1

  def get(x: Int, y: Int): Int = array(x * size + y)
  def set(x: Int, y: Int, color: Int): Unit = array(x * size + y ) = color

  def newCopy: Board = {
    new Board(array.clone, size)
  }

  def solved: Boolean = connectedCount == size * size

  def connectedRegions: Int = {
    val visited = mutable.Set.empty[Point]

    def findNotVisited: Option[Point] = {
      (for { x <- 0 until size
        y <- 0 until size
      } yield Point(x, y)).find(p => !visited.contains(p))
    }

    @tailrec
    def rec(point: Point, accum: Int): Int = {
      recFlood(point, get(point.x, point.y), visited, _ => ())

      findNotVisited match {
        case Some(nextPoint) => rec(nextPoint, accum + 1)
        case None => accum
      }
    }

    rec(Point(0, 0), 1)
  }

  def connectedCount: Int = {
    val visited = mutable.Set.empty[Point]
    recFlood(Point(0, 0), get(0, 0), visited, _ => ())
    visited.size
  }

  def flood(newColor: Int): Board = {
    val dest = newCopy

    val colorToChange = get(0, 0)
    val visited = mutable.Set.empty[Point]

    def action(p: Point): Unit = dest.set(p.x, p.y, newColor)

    recFlood(Point(0, 0), colorToChange, visited, action)
    dest
  }

  private[flood] def recFlood(p: Point, sourceColor: Int, visited: mutable.Set[Point], visitor: Point => Unit): Unit = {
    def visit(x: Int, y: Int) = {
      if (x >= 0 && x < size && y >= 0 && y < size && get(x, y) == sourceColor){
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
    size == that.size && util.Arrays.equals(array, that.array)
  }

  override def hashCode(): Int = util.Arrays.hashCode(array)

  override def toString(): String = array.grouped(size).map(_.mkString(", ")).mkString("\n")

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
    new Board(board.flatten, board.length)
  }

  def random(size: Int, numColors: Int) = {
    val board = empty(size)
    for { x <- 0 until size; y <- 0 until size } { board.set(x, y, Random.nextInt(numColors)) }
    board
  }

  def empty(size: Int): Board = new Board(Array.ofDim[Int](size * size), size)
}
