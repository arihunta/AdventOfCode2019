package arihunta.advent2019

import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue

import scala.collection.mutable.Set
import scala.io.Source
import scala.math.BigInt
import scala.collection.mutable.Map

object Day15 {

    def _01() = {

        val robot = new Robot()

        def solve(direction: Int, pathLength: Int): Int = {

            val previousCoordinates = robot.position

            val response = robot.move(direction)

            if (response == 1) {

                val x = (1 to 4)
                    .filter { _ != backwards(direction) } // TODO this only works because there are no loops, right?
                    .map { solve(_, pathLength + 1) }
                    .filter { _ != -1 }

                robot.move(backwards(direction))

                if (!x.isEmpty) x.max else -1

            } else if (response == 2) {

                robot.move(backwards(direction))

                pathLength

            } else -1

        }

        (1 to 4)
            .map { solve(_, 1) }
            .filter { _ != -1 }
            .min

    }

    def _02() = {

        val robot = new Robot()

        def goToAirVent(direction: Int): Int = {

            val previousCoordinates = robot.position

            val response = robot.move(direction)

            if (response == 1) {

                val one = if (1 != backwards(direction)) goToAirVent(1) else -1
                val two = if (one == 1) 1 else if (2 != backwards(direction)) goToAirVent(2) else -1
                val three = if (two == 1) 1 else if (3 != backwards(direction)) goToAirVent(3) else -1
                val four = if (three == 1) 1 else if (4 != backwards(direction)) goToAirVent(4) else -1

                if (four == -1) robot.move(backwards(direction))

                four

            } else if (response == 2) 1
            else -1

        }

        (1 to 4)
            .map { goToAirVent(_) }
            .max

        val distances: Map[(Int, Int), Int] = Map()

        def solve(direction: Int, pathLength: Int): Unit = {

            val previousCoordinates = robot.position

            val response = robot.move(direction)

            if (!distances.contains(robot.position) || distances(robot.position) > pathLength) distances(robot.position) = pathLength

            if (response == 1) {

                val x = (1 to 4)
                    .filter { _ != backwards(direction) } // TODO this only works because there are no loops, right?
                    .foreach { solve(_, pathLength + 1) }

                robot.move(backwards(direction))

            }

        }

        (1 to 4)
            .foreach { solve(_, 1) }

        distances.values.max

    }

    class Robot(debug: Boolean = false) {

        var position = (0, 0)

        private val in: BlockingQueue[BigInt] = new LinkedBlockingQueue[BigInt]
        private val out: BlockingQueue[BigInt] = new LinkedBlockingQueue[BigInt]
        private val program = Source.fromResource("15").mkString.trim.split(",").map(BigInt(_))
        private val computer = new IntCodeComputer(program, in, out)

        computer.runOnThread()

        private var bottomLeft: (Int, Int) = (0, 0)
        private var topRight: (Int, Int) = (0, 0)

        private val walls: Set[(Int, Int)] = Set()
        private val floors: Set[(Int, Int)] = Set((0, 0))
        private val goal: Set[(Int, Int)] = Set()

        def move(direction: Int): Int = {

            if (direction < 1 || direction > 4) return -1

            in.add(direction)
            val response = out.take().intValue()

            if (response == 0) {
                walls.add(newCoordinates(position, direction))
            } else if (response == 1) {
                position = newCoordinates(position, direction)
                floors.add(position)
            } else if (response == 2) {
                position = newCoordinates(position, direction)
                goal.add(position)
            }

            bottomLeft = (math.min(bottomLeft._1, position._1), math.min(bottomLeft._2, position._2))
            topRight = (math.max(topRight._1, position._1), math.max(topRight._2, position._2))
            if (debug) draw

            response

        }

        def draw = {
            println(
                (topRight._2 + 1 to bottomLeft._2 - 1 by -1).map { y =>
                    (bottomLeft._1 - 1 to topRight._1 + 1).map { x =>
                        val c = (x, y)
                        if (position.equals(c)) "@"
                        else if (goal.contains(c)) "!"
                        else if (walls.contains(c)) "#"
                        else if (floors.contains(c)) "."
                        else " "
                    }.mkString
                }.mkString("\n"))
            println("--------------")
        }

    }

    private def newCoordinates(currentCoordinate: (Int, Int), direction: Int): (Int, Int) =
        (currentCoordinate._1 + directions(direction)._1, currentCoordinate._2 + directions(direction)._2)

    private def backwards(direction: Int): Int =
        if (direction == 1 || direction == 3) direction + 1 else direction - 1

    private def directions(direction: Int): (Int, Int) = direction match {
        case 1 => (0, 1) // north
        case 2 => (0, -1) // south
        case 3 => (-1, 0) // west
        case 4 => (1, 0) // east
        case _ => (0, 0)
    }

}
