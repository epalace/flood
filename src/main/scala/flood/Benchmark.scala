package flood

object Benchmark {

  def main(args: Array[String]): Unit = {
    val board = Board.random(15, 4)
    println(board)
    println()

    val greedy = Solver.greedy(board)
    val (astarOptimal, aStarVisitedOptimal) = Solver.aStar(board)
    val (astarHeuristic1, aStarVisitedHeuristic1) = Solver.aStar(board, _.connectedRegions)
    println(s"Greedy: ${greedy.size} ${greedy}")
    println(s"AStar Optimal : ${astarOptimal.size} visited: ${aStarVisitedOptimal} ${astarOptimal}")
    println(s"AStar Heuristic1: ${astarHeuristic1.size} visited: ${aStarVisitedHeuristic1} ${astarHeuristic1}")
  }

}
