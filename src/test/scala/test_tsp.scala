import org.expr.rkga._
import org.expr.tsp._

class TSPTests extends munit.FunSuite {
  test("Simple Route with 5 nodes") {
    val distanceMatrix = List(
      List(0, 1, 2, 3, 4.0),
      List(1, 0, 1, 2, 3.0),
      List(2, 1, 0, 1, 2.0),
      List(3, 2, 1, 0, 1.0),
      List(4, 3, 2, 1, 0.0)
    )

    val bestcost = 8.0

    val tspresult = tsp(distanceMatrix, 100, 0.9, 0.1, 1, 100)
    val route = tspresult.route
    val cost = tspresult.cost
    assertEquals(cost, bestcost)
  }

  test("Simple Route with 5 nodes") {
    val distanceMatrix = List(
        List(4.0, 3, 7, 11, 9, 8),
        List(3.0, 5, 9, 13, 7, 5),
        List(7.0, 9, 6, 5, 12, 1),
        List(11.0, 23, 1, 3, 21),
        List(16.0, 17, 22, 33, 3)
    )

    val bestcost = 44.0 // Length of the optimal route

    val tspresult = tsp(distanceMatrix, 100, 0.9, 0.1, 1, 500)
    val route = tspresult.route
    val cost = tspresult.cost

    assertEquals(cost, bestcost)
  }
}