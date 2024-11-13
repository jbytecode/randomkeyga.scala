package org.expr 

import org.expr.rkga._ 

object tsp:

    type DistanceMatrix = Vector[Vector[Double]]

    case class TspResult(route: Vector[Int], cost: Double)

    def tsp(matrix: DistanceMatrix, popsize: Int, crosspob: Double, mutprob: Double, elitism: Int, maxiter: Int): TspResult =
        
        def costfn(x: Vector[Int]): Double =
            val pairs = (x :+ x.head).sliding(2).toVector
            val costs = for i <- pairs yield matrix(i(0))(i(1))
            costs.sum
        
        val chsize = matrix.length
        val pop = make_random_population(popsize, chsize)
        val params = GAParams(costfn, crosspob, mutprob, elitism, maxiter)
        val result = ga(pop, params)

        val tspresult = TspResult(observe(result.chromosomes.head.genes), result.chromosomes.head.cost)
        tspresult

