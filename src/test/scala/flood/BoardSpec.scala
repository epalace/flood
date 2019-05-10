package flood

import org.specs2.mutable.Specification

class BoardSpec extends Specification {
  "Board" >> {

    "fromString" >> {
      val board = Board.fromString(
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
      val board = Board.fromString(
        """
          |1, 1, 1, 1, 1
          |0, 0, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      val (actual, colored) = board.flood(2)

      val expected = Board.fromString(
        """
          |2, 2, 2, 2, 2
          |0, 0, 0, 0, 2
          |2, 2, 2, 2, 2
          |2, 0, 2, 0, 2
          |2, 2, 2, 2, 2
        """.stripMargin)

      (actual must_== expected) and (colored must_== 19)
    }

    "flood 2" >> {
      val board = Board.fromString(
        """
          |1, 1, 1, 1, 1, 1, 1
          |1, 0, 0, 0, 0, 1, 1
          |1, 1, 0, 1, 0, 1, 1
          |0, 0, 0, 0, 0, 0, 1
          |1, 1, 1, 1, 1, 0, 1
          |1, 0, 1, 0, 0, 0, 1
          |1, 1, 1, 1, 1, 1, 1
        """.stripMargin)

      val (actual, colored) = board.flood(3)

      val expected = Board.fromString(
        """
          |3, 3, 3, 3, 3, 3, 3
          |3, 0, 0, 0, 0, 3, 3
          |3, 3, 0, 1, 0, 3, 3
          |0, 0, 0, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 0, 3
          |3, 0, 3, 0, 0, 0, 3
          |3, 3, 3, 3, 3, 3, 3
        """.stripMargin)

      (actual must_== expected) and (colored must_== 31)
    }

    "flood 3" >> {
      val board = Board.fromString(
        """
          |1, 0, 1, 1, 1
          |0, 1, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      val (actual, colored) = board.flood(2)

      val expected = Board.fromString(
        """
          |2, 0, 1, 1, 1
          |0, 1, 0, 0, 1
          |1, 1, 1, 1, 1
          |1, 0, 1, 0, 1
          |1, 1, 1, 1, 1
        """.stripMargin)

      (actual must_== expected) and (colored must_== 1)
    }

  }
}
