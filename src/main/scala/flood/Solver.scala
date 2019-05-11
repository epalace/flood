package flood

import scala.annotation.tailrec
import scala.collection.mutable

object Solver {

  def greedy(board: Board): Seq[Int] = {
    val numColors = board.numColors
    @tailrec
    def recGreedy(board: Board, floodCount: Int, accum: Vector[Int]): Vector[Int] = {
      if (floodCount == board.size * board.size) {
        accum
      } else {
        val (nextBoard, bestColor, maxFloodCount) = (for {
          color <- 0 until numColors if color != board.matrix(0)(0)
        } yield {
          val newBoard = board.flood(color)
          val floodCount = newBoard.connectedCount

          (newBoard, color, floodCount)
        }).maxBy { case (_, color, floodCount) => (floodCount, -color) }

        recGreedy(nextBoard, maxFloodCount, accum :+ bestColor)
      }
    }
    recGreedy(board, board.connectedCount, Vector.empty)
  }

  def aStar(board: Board, heuristic: Board => Int = _ => 0): (Seq[Int], Int) = {
    val numColors = board.numColors
    val closedSet = mutable.Set.empty[Board]
    val openSet = mutable.PriorityQueue.empty[(Board, Vector[Int])](ordering(heuristic))

    @tailrec
    def recAStar: Vector[Int] = {
      val (board, mv) = openSet.dequeue()
      if (closedSet.contains(board)) {
        recAStar
      } else {
        closedSet += board
        if (board.connectedCount == board.size * board.size) {
          mv
        } else {
          val newNodes = for {
            color <- 0 until numColors if color != board.matrix(0)(0)
          } yield {
            val newBoard = board.flood(color)
            (newBoard, mv :+ color)
          }

          openSet ++= newNodes
          recAStar
        }
      }
    }

    openSet +=((board, Vector.empty))
    (recAStar, closedSet.size)
  }

  private[flood] def ordering(heuristic: Board => Int) =
    Ordering.by[(Board, Vector[Int]), (Int, Iterable[Int])] {
    case (board, movements) => (movements.size + heuristic(board), movements) // order them by minimum colors in case of ties
  }(Ordering.Tuple2(Ordering.Int, Ordering.Iterable[Int])).reverse


}
