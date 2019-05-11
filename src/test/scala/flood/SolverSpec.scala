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

//    "greedy random " >> {
//      val board = Board.random(15, 4)
//
//      println(board)
//      val greedy = Solver.greedy(board)
//      val (astar2, aStarVisited2) = Solver.aStar(board, _.numComponents)
//      val (astar, aStarVisited) = Solver.aStar(board)
//      println(s"Greedy: ${greedy.size} ${greedy}")
//      println(s"AStar: ${astar.size} ${astar}")
//      println(s"AStar visited: ${aStarVisited}")
//      println(s"AStar2: ${astar2.size} ${astar2}")
//      println(s"AStar2 visited: ${aStarVisited2}")
//
//      true
//    }

  }
}
