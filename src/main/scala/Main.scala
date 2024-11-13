import org.expr.tsp._
@main def hello(): Unit =
  

  val distanceMatrix = Vector(
      Vector(0, 10, 20, 30, 40.0),
      Vector(10, 0, 10, 20, 30.0),
      Vector(20, 10, 0, 10, 20.0),
      Vector(30, 20, 10, 0, 10.0),
      Vector(40, 30, 20, 10, 0.0)
    )

  val popsize = 100
  val crossoverprob = 0.9
  val mutationprob = 0.1
  val elitism = 10
  val maxiter = 5000

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
  


