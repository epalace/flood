package flood

import scala.annotation.tailrec

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
          val floodCount = newBoard.floodCount

          (newBoard, color, floodCount)
        }).maxBy { case (_, color, floodCount) =>
          (floodCount, -color)

        }

        recGreedy(nextBoard, maxFloodCount, accum :+ bestColor)
      }
    }
    recGreedy(board, board.floodCount, Vector.empty)
  }

}
