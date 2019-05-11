package flood

import org.specs2.mutable.Specification

class SolverSpec extends Specification {
  "Solver" >> {

    "greedy trivial" >> {
      Solver.greedy(Board.parse("1")) must beEmpty
    }

    "greedy trivial (2) " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
        """.stripMargin)
      Solver.greedy(board) must beEmpty
    }


    "greedy 1 change " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 0, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      Solver.greedy(board) must_==Seq(0)
    }

    "greedy 2 change " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 2, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      Solver.greedy(board) must_==Seq(0, 2)
    }

    "greedy 3 change " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 0, 1
          |1, 1, 1, 0, 2
          |1, 1, 1, 0, 0
          |1, 1, 2, 2, 2
          |1, 1, 2, 2, 2
        """.stripMargin)

      Solver.greedy(board) must_==Seq(2, 0, 1, 2)
    }

    "greedy random " >> {
      val board = Board.random(10, 10)
      println(board)
      val movements = Solver.greedy(board)
      println(movements)

      movements.foldLeft(board) { case (board, nextColor) =>
        val nextBoard = board.flood(nextColor)
        println(nextBoard)
        println()
        nextBoard
      }

      true
    }

  }
}
