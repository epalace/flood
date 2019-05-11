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

    "A* trivial" >> {
      Solver.greedy(Board.parse("1")) must beEmpty
    }

    "A* trivial (2) " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
        """.stripMargin)
      val (actual, numVisited) = Solver.aStar(board)

      actual must beEmpty
    }


    "A* 1 change " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 0, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      val (actual, numVisited) = Solver.aStar(board)
      actual must_==Seq(0)
    }

    "A* 2 change " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 1, 1, 1
          |1, 1, 2, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      val (actual, numVisited) = Solver.aStar(board)

      actual must_==Seq(0, 2)
    }

    "A* 3 change " >> {
      val board = Board.parse(
        """
          |1, 1, 1, 0, 1
          |1, 1, 1, 0, 2
          |1, 1, 1, 0, 0
          |1, 1, 2, 2, 2
          |1, 1, 2, 2, 2
        """.stripMargin)

      val (actual, numVisited) = Solver.aStar(board)

      actual must_==Seq(0, 1, 2)
      numVisited must_== 9
    }
  }
}
