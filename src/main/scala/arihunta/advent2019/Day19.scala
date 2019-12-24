package arihunta.advent2019

import scala.io.Source

object Day19 {

    val program = Source.fromResource("19").mkString.trim.split(",").map { BigInt(_) }.toArray

    def _01() = {

        (0 until 50).flatMap { x =>
            (0 until 50).map { y =>
                testCoordinate(x, y)
            }
        }
            .count { output => output }

    }

    def _02() = {

        var row = 5
        val beamWidthAt5 = beamWidthBinarySearch(5, (0, 10))

        var x = beamWidthAt5._1
        var y = beamWidthAt5._2

        while (y - x != 100) {
            row += 100 - (y - x)
            while (!testCoordinate(x, row)) x += 1
            y = if (x > y) x else y
            while (testCoordinate(y, row)) y += 1
        }

        while (!testCoordinate(x, row + 99)) x += 1

        while (y - x != 100) {
            row += 100 - (y - x)
            while (!testCoordinate(x, row + 99)) x += 1
            y = if (x > y) x else y
            while (testCoordinate(y, row)) y += 1
        }

        x * 10000 + row

    }

    def testCoordinate(x: Int, y: Int): Boolean = {
        val computer = new IntCodeComputer(program)
        computer.runOnThread()
        computer.input.add(BigInt(x))
        computer.input.add(BigInt(y))
        computer.output.take() == 1
    }

    def beamWidthBinarySearch(row: Int, range: (Int, Int)): (Int, Int) = {

        if (range._1 >= range._2 - 1) return (-1, -1)

        val centre = (range._2 + range._1) / 2

        if (testCoordinate(centre, row))
            beamWidthAtRow(row, centre)

        else {
            val leftSearch = beamWidthBinarySearch(row, (range._1, centre))
            if (leftSearch != (-1, -1)) leftSearch
            else beamWidthBinarySearch(row, (centre, range._2))
        }

    }

    def beamWidthAtRow(row: Int, col: Int): (Int, Int) = {

        var leftEdge = col - 1
        var rightEdge = col + 1

        while (testCoordinate(leftEdge, row)) leftEdge -= 1
        while (testCoordinate(rightEdge, row)) rightEdge += 1

        (leftEdge + 1, rightEdge - 1)
    }

}
