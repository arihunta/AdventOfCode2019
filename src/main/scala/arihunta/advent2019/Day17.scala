package arihunta.advent2019

import scala.io.Source
import java.util.concurrent.TimeUnit
import scala.collection.mutable.Set
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue

object Day17 {

    def _01() = {

        val program = Source.fromResource("17").mkString.trim.split(",").map { BigInt(_) }.toArray

        val image: BlockingQueue[BigInt] = new LinkedBlockingQueue

        new IntCodeComputer(program, output = image).run()

        val planks: Set[(Int, Int)] = Set()

        var x = 0
        var y = 0
        var stillReading = true

        var maxX = 0

        while (!image.isEmpty()) {
            val next = image.take

            // if (next != null) print(next.toChar)

            if (next == null) stillReading = false
            else if (next == 46) {
                x += 1
            } else if (next == 10) {
                x = 0
                y += 1
            } else {
                planks.add((x, y))
                x += 1
            }
            maxX = math.max(maxX, x)
        }

        planks.filter {
            coord =>
                planks.contains((coord._1 - 1, coord._2)) &&
                    planks.contains((coord._1 + 1, coord._2)) &&
                    planks.contains((coord._1, coord._2 - 1)) &&
                    planks.contains((coord._1, coord._2 + 1))
        }
            .map(coord => coord._1 * coord._2).sum

    }

    def _02(): Int = {

        val program = Source.fromResource("17").mkString.trim.split(",").map { BigInt(_) }.toArray
        program(0) = 2

        val computer = new IntCodeComputer(program)
        computer.runOnThread()

        val main = "A,C,A,C,B,A,B,A,B,C"
        val a = "R,12,L,8,L,4,L,4"
        val b = "L,8,L,4,R,12,L,6,L,4"
        val c = "L,8,R,6,L,6"

        val input = main + "\n" + a + "\n" + b + "\n" + c + "\n" + "n" + "\n"

        input.chars.forEach(computer.input.add(_))

        while (true) {
            val out = computer.output.take
            if (out > 127) return out.intValue()
        }

        -1

    }

    private def simulate(main: String, a: String, b: String, c: String) {

        val program = main
            .replaceAll("A", a)
            .replaceAll("B", b)
            .replaceAll("C", c)
            .split(",")

        var coords = (0, 0)
        var heading = 0

        var bottomLeft: (Int, Int) = (0, 0)
        var topRight: (Int, Int) = (0, 0)

        val path: Set[(Int, Int)] = Set((0, 0))

        for (instruction <- program) {

            if (instruction == "R") {
                heading = (heading + 1) % 4
            } else if (instruction == "L") {
                heading = if (heading == 0) 3 else heading - 1
            } else {
                (1 to instruction.toInt).foreach { _ =>
                    coords = newCoordinates(coords, heading)
                    path.add(coords)
                    bottomLeft = (math.min(bottomLeft._1, coords._1), math.min(bottomLeft._2, coords._2))
                    topRight = (math.max(topRight._1, coords._1), math.max(topRight._2, coords._2))
                }
            }

        }

        draw(path, bottomLeft, topRight)

    }

    private def draw(path: Set[(Int, Int)], bottomLeft: (Int, Int), topRight: (Int, Int)) = {

        val image = path.map { it => (it._1 - bottomLeft._1, it._2 - bottomLeft._2) }
        val newTopRight = (topRight._1 - bottomLeft._1, topRight._2 - bottomLeft._2)

        println(
            (newTopRight._2 + 1 to bottomLeft._2 - 1 by -1).map { y =>
                (bottomLeft._1 - 1 to newTopRight._1 + 1).map { x =>
                    if (image.contains((x, y))) "#"
                    else " "
                }.mkString
            }.mkString("\n"))

    }

    private def newCoordinates(currentCoordinate: (Int, Int), direction: Int): (Int, Int) =
        (currentCoordinate._1 + directions(direction)._1, currentCoordinate._2 + directions(direction)._2)

    private def backwards(direction: Int): Int =
        if (direction == 1 || direction == 3) direction + 1 else direction - 1

    private def directions(direction: Int): (Int, Int) = direction match {
        case 0 => (0, 1)
        case 1 => (1, 0)
        case 2 => (0, -1)
        case 3 => (-1, 0)
        case _ => (0, 0)
    }

}
