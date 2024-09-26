# randomkeyga.scala
Random Key Genetic Algorithms for Permutation Optimization in Scala


# Example

## Traveling Salesman Example

```scala
import org.expr.tsp._

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
```