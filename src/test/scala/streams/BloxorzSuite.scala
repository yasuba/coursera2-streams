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
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
    }
  }

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


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1,1))
    }
  }

  test("findChar goal level 1") {
    new Level1 {
      assert(goal == Pos(4,7))
    }
  }

  test("neighbours with history") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(1,1),Pos(1,1)), List(Left,Up)).toSet == Set(
        (Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)),
        (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up)))
      )
    }
  }


  test("neighbours with history no legal moves") {
    new Level1 {
      assert(neighborsWithHistory(Block(Pos(5,1),Pos(5,1)), List()).toSet == Set())
    }
  }

  test("new neighbours only") {
    new Level1 {
      assert(newNeighborsOnly(
        Set((Block(Pos(1,2),Pos(1,3)), List(Right,Left,Up)), (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream,
        Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1))))
        == Set((Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream)
    }
  }

  test("from function") {
    new Level1 {
      assert(
        from(
          Set((Block(Pos(1,2),Pos(1,3)), List(Left,Up)), (Block(Pos(2,1),Pos(3,1)), List(Down,Left,Up))).toStream,
          Set(Block(Pos(1,2),Pos(1,3)), Block(Pos(1,1),Pos(1,1)))
        )
          == Set((Block(Pos(1,4),Pos(1,4)),List(Right, Left, Up)), (Block(Pos(2,2),Pos(2,3)),List(Down, Left, Up)), (Block(Pos(2,2),Pos(3,2)),List(Right, Down, Left, Up))).toStream
      )
    }
  }

  test("paths from start") {
    new Level1 {
      assert(
        from(Set((startBlock, List())).toStream,
          Set()
        )
          == Stream((Block(Pos(1,2),Pos(1,3)),List(Right)), (Block(Pos(2,1),Pos(3,1)),List(Down)))
      )
    }
  }

  test("paths to goal") {
    new Level1 {
      assert(
        pathsToGoal.toSet == Set((Block(Pos(4,7),Pos(4,7)),List(Down, Right, Right, Right, Down, Right, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Down, Right, Right, Down, Right)), (Block(Pos(4,7),Pos(4,7)),List(Right, Down, Right, Right, Down, Down, Right)))
      )
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
