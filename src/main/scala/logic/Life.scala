package logic

object Life:
  type Pos =  (Int, Int)
  type Grid = List[Pos]
  val width = 30
  val height = 30

  def isAlive(p: Pos)(g: Grid): Boolean =
    g.contains(p)

  def isEmpty(p: Pos)(g: Grid): Boolean = 
    !isAlive(p)(g)

  /** find neighbours of a position */

  def neighbours(p: Pos): List[Pos] =
    val (x,y) = p
    List(
      (x - 1, y - 1), (x - 1, y), (x -1, y + 1),
      (x,     y - 1),             (x, y + 1),
      (x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
    ).map(wrapAround)

  /** if position is ouside of the grid wraparound */

  def wrapAround(p: Pos): Pos = 
    val (x, y) = p
    val newX = ((x - 1 + width) % width) + 1
    val newY = ((y - 1 + height) % height) + 1
    (newX, newY)
  
  /** find number of alive neighbours */

  def liveNeighbours(p: Pos)(g: Grid): Int =
    neighbours(p).filter(isAlive(_)(g)).length

  /** return list of surviving cells */

  def survivors(g: Grid): List[Pos] = 
    for
      p <- g
      living = liveNeighbours(p)(g)
      if living == 2 || living == 3
    yield p
  
  /** return list of cells to be born */

  def births(g: Grid): List[Pos] = 
    for 
      p <- allNeighbours(g)
      if isEmpty(p)(g) && liveNeighbours(p)(g) == 3
    yield p

  def allNeighbours(g: Grid): List[Pos] = 
    g.flatMap(neighbours).distinct
  
  /** combine survivors and births to return 
      next starting Grid */
  def nextGeneration(g: Grid): Grid = 
    survivors(g) ::: births(g)