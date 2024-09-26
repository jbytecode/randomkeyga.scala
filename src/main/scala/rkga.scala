package org.expr 

object rkga:

  type CostFn = List[Int] => Double

  case class Chromosome(genes: List[Double], cost: Double)
  
  case class Population(chromosomes: List[Chromosome])

  case class GAParams(
      costfn: CostFn,
      crossprob: Double,
      mutprob: Double,
      elitism: Int,
      maxiter: Int
  )

  def observe(values: List[Double]): List[Int] =
    values.zipWithIndex.sortBy(_._1).map(_._2).toList

  def observe(c: Chromosome): List[Int] = observe(c.genes)

  def make_random_chromosome(chsize: Int): Chromosome =
    Chromosome(List.fill(chsize)(math.random), Double.MaxValue)

  def one_point_crossover(
      c1: Chromosome,
      c2: Chromosome,
      prob: Double
  ): Chromosome =
    if prob < Math.random then
      val point = (math.random * c1.genes.length).toInt
      val genes = c1.genes.take(point) ++ c2.genes.drop(point)
      Chromosome(genes, Double.MaxValue)
    else c1

  def one_point_crossover(pop: Population, prob: Double): Chromosome =
    val c1 = pop.chromosomes((math.random * pop.chromosomes.length).toInt)
    val c2 = pop.chromosomes((math.random * pop.chromosomes.length).toInt)
    one_point_crossover(c1, c2, prob)

  def tournament_selection(pop: Population, costfn: CostFn, tsize: Int): Chromosome =
    val chlist = List.fill(tsize)(pop.chromosomes((math.random * pop.chromosomes.length).toInt))
    val chslist_with_cost = calculate_costs(chlist, costfn)
    chslist_with_cost.minBy(_.cost)

  def random_mutation(c: Chromosome, p: Double): Chromosome =
    val genes = c.genes.map(x => if math.random < p then Math.random else x)
    Chromosome(genes, Double.MaxValue)

  def calculate_cost(c: Chromosome, costfn: CostFn): Chromosome =
    Chromosome(c.genes, costfn(observe(c)))

  def calculate_costs(
      chromosomes: List[Chromosome],
      costfn: CostFn
  ): List[Chromosome] =
    chromosomes.map(c => calculate_cost(c, costfn))

  def calculate_costs(pop: Population, costfn: CostFn): List[Chromosome] =
    calculate_costs(pop.chromosomes, costfn)

  def average_cost(pop: Population): Double =
    pop.chromosomes.map(_.cost).sum / pop.chromosomes.length

  def make_random_population(psize: Int, chsize: Int): Population =
    val chromosomes = List.fill(psize)(make_random_chromosome(chsize))
    Population(chromosomes)

  def generation(pop: Population, params: GAParams): Population =
    val costfn = params.costfn
    val sorted =
      pop.chromosomes.map(c => calculate_cost(c, costfn)).sortBy(_.cost)
    val elite = sorted.take(params.elitism)
    val rest = sorted.drop(params.elitism)
    val newpop = elite ++ rest.map(c =>
      val c1 = tournament_selection(pop, params.costfn, 2)
      val c2 = tournament_selection(pop, params.costfn, 2)
      val c3 = one_point_crossover(c1, c2, params.crossprob)
      random_mutation(c3, params.mutprob)
    )
    Population(calculate_costs(newpop, params.costfn))


  def ga(population: Population, params: GAParams): Population =
    def loop(pop: Population, i: Int): Population =
      if i == params.maxiter then pop
      else loop(generation(pop, params), i + 1)
    loop(population, 0)

  def ga(
      psize: Int,
      chsize: Int,
      costfn: CostFn,
      crossprob: Double,
      mutprob: Double,
      elitism: Int,
      maxiter: Int
  ): Population =
    val params = GAParams(costfn, crossprob, mutprob, elitism, maxiter)
    val pop = make_random_population(psize, chsize)
    ga(pop, params)
