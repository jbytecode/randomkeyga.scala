import org.expr.rkga._


class RandomKeyGATests extends munit.FunSuite {
  test("Observe random keys") {
    val x = Vector(1.0, 2, 3, 4, -1, 0)
    val y = observe(x)
    assertEquals(y, Vector(4, 5, 0, 1, 2, 3))
  }

  test("Observe Chromosome") {
    val c = Chromosome(Vector(0.1, 0.2, 0.3, 0.4, -1.0, 0.0), 0.0)
    val y = observe(c)
    assertEquals(y, Vector(4, 5, 0, 1, 2, 3))
  }

  test("Make Random Chromosome") {
    val c = make_random_chromosome(6)
    assertEquals(c.genes.length, 6)
    assertEquals(c.cost, Double.MaxValue)
    assertEquals(c.genes.forall(x => x >= 0 && x <= 1), true)
  }

  test("One Point Crossover") {
    val c1 = Chromosome(Vector(0.1, 0.2, 0.3, 0.4, -1.0, 0.0), 0.0)
    val c2 = Chromosome(Vector(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), 0.0)
    val c = one_point_crossover(Population(Vector(c1, c2)), 1.0)
    assertEquals(c.genes.length, 6)
    assertEquals(c.genes.forall(x => x >= -1.0 && x <= 1.0), true)
    val cr0 = c.genes == Vector(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
    val cr1 = c.genes == Vector(0.1, 0.6, 0.7, 0.8, 0.9, 1.0)
    val cr2 = c.genes == Vector(0.1, 0.2, 0.7, 0.8, 0.9, 1.0)
    val cr3 = c.genes == Vector(0.1, 0.2, 0.3, 0.8, 0.9, 1.0)
    val cr4 = c.genes == Vector(0.1, 0.2, 0.3, 0.4, 0.9, 1.0)
    val cr5 = c.genes == Vector(0.1, 0.2, 0.3, 0.4, -1.0, 1.0)
    val cr6 = c.genes == Vector(0.1, 0.2, 0.3, 0.4, -1.0, 0.0)
    assert(cr1 || cr2 || cr3 || cr4 || cr5 || cr6 || cr0)
  }

  test("Random mutation - zero prob") {
    val c = make_random_chromosome(6)
    val p = 0.0
    val c1 = random_mutation(c, p)
    assertEquals(c1.genes.length, 6)
    assertEquals(c1.cost, Double.MaxValue)
    assert(c1.genes.forall(x => x >= 0.0 && x <= 1.0))
    assert(c1.genes == c.genes)
  }

  test("Random mutation - prob of one - all change") {
    val c = make_random_chromosome(6)
    val p = 1.0
    val c1 = random_mutation(c, p)
    assertEquals(c1.genes.length, 6)
    assertEquals(c1.cost, Double.MaxValue)
    assert(c1.genes.forall(x => x >= 0.0 && x <= 1.0))
    assert(c1.genes != c.genes)
  }

  test("Calculate Cost of a Chromosome") {
    def costfn(x: Vector[Int]): Double =
      val expected = Vector(0, 1, 2, 3, 4, 5)
      x.zip(expected).map { case (a, b) => math.abs(a - b) }.sum

    val c = Chromosome(Vector(0, 1, 2, 3, 4, 5, 6), 0.0)
    val c1 = calculate_cost(c, costfn)
    assertEquals(c1.cost, 0.0)

    val c2 = Chromosome(Vector(6, 1, 2, 3, 4, 5), 0.0)
    val c3 = calculate_cost(c2, costfn)
    assertEquals(c3.cost, 10.0)
  }

  test("Make random population") {
    val pop = make_random_population(10, 6)
    assertEquals(pop.chromosomes.length, 10)
    assertEquals(pop.chromosomes.forall(c => c.genes.length == 6), true)
    assertEquals(pop.chromosomes.forall(c => c.cost == Double.MaxValue), true)
    assertEquals(
      pop.chromosomes.forall(c => c.genes.forall(x => x >= 0.0 && x <= 1.0)),
      true
    )
  }

  test("Tournament Selection"){
    def costfn(x: Vector[Int]): Double =
      val expected = Vector(0, 1, 2, 3, 4, 5)
      x.zip(expected).map { case (a, b) => math.abs(a - b) }.sum

    val pop = make_random_population(10, 6)
    
    // Sampling 1000 times to make sure the selected chromsome is the best and the same
    // of course, this is not a good test, but it is a simple one
    val c1 = tournament_selection(pop, costfn, 1000)
    val c2 = tournament_selection(pop, costfn, 1000)

    assert(c1.cost == c2.cost)
  }

  test("Many generations") {
    def costfn(x: Vector[Int]): Double =
      val expected = Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
      x.zip(expected).map { case (a, b) => math.abs(a - b) }.sum

    val pop = make_random_population(100, 6)
    val sortedpop =
      pop.chromosomes.map(c => calculate_cost(c, costfn)).sortBy(_.cost)

    val params = GAParams(
      costfn,
      crossprob = 0.5,
      mutprob = 0.5,
      elitism = 4,
      maxiter = 10
    )

    // Gen 1
    var pop1 = generation(pop, params)
    var i = 0
    while i < 10 do
      pop1 = generation(pop1, params)
      i += 1

    val sortedpop1_avgcost = average_cost(Population(sortedpop))
    val pop1_avgcost = average_cost(pop1)

    assert(pop1_avgcost <= sortedpop1_avgcost)
  }

  test("Full GA on a simple problem") {
    def costfn(x: Vector[Int]): Double =
      val expected = Vector(0, 1, 2, 3, 4, 5)
      x.zip(expected).map { case (a, b) => math.abs(a - b) }.sum

    val pop = make_random_population(10, 6)
    val params = GAParams(
      costfn,
      crossprob = 0.5,
      mutprob = 0.5,
      elitism = 2,
      maxiter = 100
    )
    val pop1 = ga(pop, params)
    val avgcost = pop1.chromosomes.map(_.cost).sum / pop1.chromosomes.length

    assertEquals(pop1.chromosomes.head.cost, 0.0)
    assertEquals(observe(pop1.chromosomes.head.genes), Vector(0, 1, 2, 3, 4, 5))
  }

  test("Full GA on a TSP problem") {
    val distanceMatrix = Vector(
      Vector(0, 1, 2, 3, 4.0),
      Vector(1, 0, 1, 2, 3.0),
      Vector(2, 1, 0, 1, 2.0),
      Vector(3, 2, 1, 0, 1.0),
      Vector(4, 3, 2, 1, 0.0)
    )
    def costfn(x: Vector[Int]): Double =
      val pairs = (x :+ x.head).sliding(2).toVector
      val costs = for i <- pairs yield distanceMatrix(i(0))(i(1))
      costs.sum

    val pop = make_random_population(100, 5)
    val params = GAParams(
      costfn,
      crossprob = 0.70,
      mutprob = 0.5,
      elitism = 4,
      maxiter = 1000
    )
    val pop1 = ga(pop, params)

    val bestpermutation = Vector(0, 1, 2, 3, 4)
    val bestcost = costfn(bestpermutation)

    val bestsolution = observe(pop1.chromosomes.head.genes)

    assert(pop1.chromosomes.head.cost == bestcost)
  }

}
