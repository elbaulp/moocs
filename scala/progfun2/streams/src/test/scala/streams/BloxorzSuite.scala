package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) =>
          require(block.isLegal) // The solution must always lead to legal blocks
          move match {
            case Left => block.left
            case Right => block.right
            case Up => block.up
            case Down => block.down
          }
      }
  }

  trait InfiniteChecker extends GameDef with Solver

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait Level0 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """oooooooooo
      |oSoooooooo
      |oooooooooo
      |oooooooooo
      |oooooooToo
      |oooooooooo""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  trait InfiniteLevel extends InfiniteTerrain with InfiniteChecker {
    override lazy val startPos = Pos(10,10)
    override lazy val goal = Pos(30,30)
    override val terrain: Terrain = (pos: Pos) => true
  }

  test("From in InfiniteLevel") {
    new InfiniteLevel {
      //println(newNeighborsOnly(neighborsWithHistory(startBlock, Nil), Set.empty).take(4).toList)
      //println(from(Set((startBlock, Nil)).toStream, Set.empty).take(40).toList mkString "\n")
    }
  }

  test("Neighbors with History Level 1") {
    new Level1 {
      val neigh = neighborsWithHistory(Block(Pos(1,1), Pos(1,1)), List(Left, Up))
      val sol = Set(
        (Block(Pos(1,2), Pos(1,3)), List(Right, Left, Up)),
        (Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up))
      )
      assert(neigh.toSet === sol, "Neighbors")
    }
  }

  test("new Neighbors only") {
    new Level1 {
      val news = newNeighborsOnly(
        Set(
          (Block(Pos(1,2), Pos(1,3)), List(Right, Left, Up)),
          (Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up))
        ).toStream,
        Set(Block(Pos(1,2), Pos(1,3)), Block(Pos(1,1), Pos(1,1)))
      )
      val expected = Set(
        (Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up))
      ).toStream

      assert(news === expected, "New neigh only")
    }
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(1, 1)), "1,1") // start
      assert(terrain(Pos(4, 7)), "4,7") // goal
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(6, 8)), "6,8")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(!terrain(Pos(-1, 0)), "-1,0")
      assert(!terrain(Pos(0, -1)), "0,-1")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }

}
