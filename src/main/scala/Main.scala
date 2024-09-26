import org.expr.tsp._
@main def hello(): Unit =
  

  val distanceMatrix = List(
      List(0, 1, 2, 3, 4.0),
      List(1, 0, 1, 2, 3.0),
      List(2, 1, 0, 1, 2.0),
      List(3, 2, 1, 0, 1.0),
      List(4, 3, 2, 1, 0.0)
    )

  val popsize = 100
  val crossoverprob = 0.9
  val mutationprob = 0.1
  val elitism = 1
  val maxiter = 100

  val tspresult = tsp(distanceMatrix, 
                    popsize, 
                    crossoverprob, 
                    mutationprob, 
                    elitism, 
                    maxiter)

  val route = tspresult.route
  val cost = tspresult.cost
  Console.println(Console.YELLOW + "The Random Key Genetic Algorithm - Example")
  Console.println(Console.RED + "===========================================")
  Console.println(Console.GREEN + "Distance Matrix:")
  distanceMatrix.foreach(row => Console.println(Console.MAGENTA + row.mkString(" ")))
  Console.println(Console.GREEN + s"Route: $route")
  Console.println(Console.GREEN + s"Cost: $cost")
  


