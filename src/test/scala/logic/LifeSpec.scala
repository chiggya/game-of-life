package logic
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.PrivateMethodTester


class LifeSpec extends AnyFlatSpec with PrivateMethodTester:
  import Life.*
  type Pos = (Int,Int)
  type Grid = List[Pos]
  val height = 30
  val width = 30
  val p1: Pos = (2,3)
  val p2: Pos = (11,12)
  val g: Grid = List((1,2), (1,4), (2,1), (2,3), (3,2))
  
  "A grid g containing a position p" should "return true" in {
    
    assert(isAlive(p1)(g))
    assert(!isAlive(p2)(g))
  }

  "A position not contained in the grid" should "return false" in {
    assert(!isEmpty(p1)(g))  
    assert(isEmpty(p2)(g))
  }

  "calling neighbours" should "return a list of neighbours" in {
    val neighbs = List(
        (1,2), (1,3), (1, 4),
        (2,2),        (2,4),
        (3,2), (3,3), (3,4)
      )
    assert(neighbours(p1) == neighbs)
  }

  "positions outside the boundary" should "wrap back around" in {
    val pastBoundary: Pos = (31,31)
    val beforeBoundary: Pos = (0, 0)

    assert(wrapAround(pastBoundary) == (1,1))
    assert(wrapAround(beforeBoundary) == (30,30))
    
  }

  "looking at neighbouring positions" should "return the number of living positions" in {
    val p: Pos = (1,2)
    assert(liveNeighbours(p)(g) == 2)
  }

  "positions with 2 or 3 neighbours" should "survive for next generation" in {
    val gSurvivors: List[Pos] =
      List((1,2), (2,1), (2,3), (3,2))
    assert(survivors(g) == gSurvivors)
  }

  "empty positions in grid that have 3 living neighbours" should "result in a living position" in {
    val newBirths: List[Pos] = List((1, 3))
    assert(births(g) == newBirths)
  }

  "combining survivors and births" should "return the next generation of cells" in {
    val nextGen = List((1,2), (2,1), (2,3), (3,2), (1,3))
    assert(nextGeneration(g) == nextGen)
  }