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
          val floodCount = newBoard.floodCount

          (newBoard, color, floodCount)
        }).maxBy { case (_, color, floodCount) => (floodCount, -color) }

        recGreedy(nextBoard, maxFloodCount, accum :+ bestColor)
      }
    }
    recGreedy(board, board.floodCount, Vector.empty)
  }

  val ordering:Ordering[(Board, Vector[Int])] = Ordering.by[(Board, Vector[Int]), (Int, Iterable[Int])] {
    case (board, movements) => (movements.size, movements)
  }(Ordering.Tuple2(Ordering.Int, Ordering.Iterable[Int])).reverse

//  def ordering = Ordering.fromLessThan[(Board, Vector[Int])] {
//    case (a, b) =>
//      a._2.size < b._2.size
////      if (a._2.size < b._2.size) {
////        true
////      } else {
////        Ordering.Iterable[Int].lteq(a._2, b._2)
////      }
//  }.reverse

  def AStar(board: Board): Seq[Int] = {
    val numColors = board.numColors
    val closedSet = mutable.Set.empty[Board]
    val openSet = mutable.PriorityQueue.empty[(Board, Vector[Int])](ordering)

    @tailrec
    def recAStar: Vector[Int] = {
      //println(s"QUEUE:\n${openSet.toList.map(_._2).mkString("\n")}\n")
      val (board, mv) = openSet.dequeue()
      //println(s"DEQUEUED ${mv}")
      if (closedSet.contains(board)) {
        recAStar
      } else {
        closedSet += board
        if (board.floodCount == board.size * board.size) {
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
    recAStar
  }

}
