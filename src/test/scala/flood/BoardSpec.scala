package flood

import org.specs2.mutable.Specification

class BoardSpec extends Specification {
  "Board" >> {

    "fromString" >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |0, 0, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin
      )

      board.matrix must_== Array(
        Array(1, 1, 1, 1, 1),
        Array(0, 0, 0, 0, 1),
        Array(1, 1, 1, 1, 1),
        Array(1, 0, 1, 0, 1),
        Array(1, 1, 1, 1, 1)
      )

    }

    "flood 1" >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1
          |0, 0, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      val actual = board.flood(2)

      val expected = Board.parse(
        """
          |2, 2, 2, 2, 2
          |0, 0, 0, 0, 2
          |2, 2, 2, 2, 2
          |2, 0, 2, 0, 2
          |2, 2, 2, 2, 2
        """.stripMargin)

      (actual must_== expected)
    }

    "flood 2" >> {
      val board = Board.parse(
        """
          |1, 1, 1, 1, 1, 1, 1
          |1, 0, 0, 0, 0, 1, 1
          |1, 1, 0, 1, 0, 1, 1
          |0, 0, 0, 0, 0, 0, 1
          |1, 1, 1, 1, 1, 0, 1
          |1, 0, 1, 0, 0, 0, 1
          |1, 1, 1, 1, 1, 1, 1
        """.stripMargin)

      val actual = board.flood(3)

      val expected = Board.parse(
        """
          |3, 3, 3, 3, 3, 3, 3
          |3, 0, 0, 0, 0, 3, 3
          |3, 3, 0, 1, 0, 3, 3
          |0, 0, 0, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 0, 3
          |3, 0, 3, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 3, 3
        """.stripMargin)

      (actual must_== expected)
    }

    "flood 3" >> {
      val board = Board.parse(
        """
          |1, 0, 1, 1, 1
          |0, 1, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      val actual = board.flood(2)

      val expected = Board.parse(
        """
          |2, 0, 1, 1, 1
          |0, 1, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      (actual must_== expected)
    }

    "floodCount 1" >> {
      val board = Board.parse(
        """
          |3, 3, 3, 3, 3, 3, 3
          |3, 0, 0, 0, 0, 3, 3
          |3, 3, 0, 1, 0, 3, 3
          |0, 0, 0, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 0, 3
          |3, 0, 3, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 3, 3
        """.stripMargin)

      board.connectedCount must_== 31
    }

    "num components (1)" >> {
      val board = Board.parse(
        """
          |3, 3, 3, 3, 3, 3, 3
          |3, 0, 0, 4, 0, 3, 3
          |3, 3, 0, 1, 0, 3, 3
          |0, 0, 2, 2, 2, 0, 3
          |3, 3, 3, 3, 3, 0, 3
          |3, 0, 3, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 3, 3
        """.stripMargin)

      board.connectedRegions must_== 9
    }

  }
}
