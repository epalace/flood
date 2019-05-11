package flood

object Benchmark {

  def main(args: Array[String]): Unit = {
    //val board = Board.random(20, 4)
    val board = Board.parse(
      """
        |1, 0, 1, 1, 1, 0, 2, 3, 2, 2, 0, 2, 3, 0, 2
        |3, 3, 1, 0, 0, 3, 3, 1, 0, 2, 2, 0, 2, 1, 0
        |1, 1, 3, 3, 3, 2, 1, 0, 2, 2, 1, 3, 2, 3, 1
        |3, 3, 3, 1, 0, 3, 1, 0, 1, 2, 2, 0, 0, 1, 2
        |3, 1, 2, 0, 3, 0, 0, 1, 0, 1, 3, 1, 0, 1, 0
        |2, 3, 0, 0, 2, 3, 1, 1, 0, 2, 1, 1, 2, 1, 2
        |0, 3, 0, 2, 2, 0, 1, 0, 0, 1, 1, 0, 2, 1, 0
        |0, 2, 2, 3, 1, 0, 2, 3, 1, 1, 1, 1, 1, 2, 1
        |1, 3, 3, 1, 0, 0, 2, 3, 0, 2, 2, 1, 3, 0, 3
        |2, 3, 0, 3, 1, 3, 0, 0, 1, 0, 3, 0, 0, 1, 2
        |3, 2, 2, 0, 0, 2, 1, 1, 3, 0, 3, 1, 3, 1, 3
        |3, 1, 3, 3, 3, 2, 1, 2, 3, 3, 2, 1, 2, 0, 0
        |1, 1, 2, 3, 0, 1, 2, 3, 3, 1, 0, 3, 0, 0, 0
        |0, 0, 1, 3, 0, 0, 0, 3, 2, 2, 3, 2, 0, 2, 1
        |3, 2, 2, 1, 2, 0, 0, 3, 1, 3, 3, 2, 2, 1, 2
      """.stripMargin)

    println(board)
    println()

    val greedy = Solver.greedy(board)
    val (optimal, optimalVisted, optimalHepMaxSize) = Solver.aStar(board)
    val (heuristic, heuristicVisited, heuristicHeapMaxSize) = Solver.aStar(board, _.connectedRegions)
    println(s"Greedy: ${greedy.size} ${greedy}")
    println(s"AStar Optimal : ${optimal.size} visited: ${optimalVisted} maxHeapSize: ${optimalHepMaxSize} ${optimal}")
    println(s"AStar Heuristic1: ${heuristic.size} visited: ${heuristicVisited} maxHeapSize: ${heuristicHeapMaxSize} ${heuristic}")
  }

}
