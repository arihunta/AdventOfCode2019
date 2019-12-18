package arihunta.advent2019

import scala.io.Source
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.Set
import java.util.HashSet
import java.util.concurrent.TimeUnit

object Day11 {

    val headings = Array((0, 1), (1, 0), (0, -1), (-1, 0))

    def _01() = {

        val program = Source.fromResource("11").mkString.trim.split(",").map(it => BigInt(it))

        val input: BlockingQueue[BigInt] = new LinkedBlockingQueue
        val output: BlockingQueue[BigInt] = new LinkedBlockingQueue

        val computer = new IntCodeComputer(program, input, output)
        computer.runOnThread()

        var currentPosition = (0, 0)
        var currentHeading = 0

        val visited: Set[Tuple2[Int, Int]] = new HashSet()
        val white: Set[Tuple2[Int, Int]] = new HashSet()
        visited.add(currentPosition)

        while (!computer.isFinished()) {

            input.put(if (white.contains(currentPosition)) 1 else 0)

            val color = output.poll(100, TimeUnit.MILLISECONDS)
            if (color != null) {
                if (color == 1) white.add(currentPosition)
                else white.remove(currentPosition)

                val direction = output.take()
                if (direction == 1) currentHeading = (currentHeading + 1) % headings.size
                else currentHeading = if (currentHeading == 0) 3 else currentHeading - 1

                currentPosition = (currentPosition._1 + headings(currentHeading)._1, currentPosition._2 + headings(currentHeading)._2)
                visited.add(currentPosition)
            }

        }

        visited.size()

    }

    def _02() = {

        val program = Source.fromResource("11").mkString.trim.split(",").map(it => BigInt(it))

        val input: BlockingQueue[BigInt] = new LinkedBlockingQueue
        val output: BlockingQueue[BigInt] = new LinkedBlockingQueue

        val computer = new IntCodeComputer(program, input, output)
        computer.runOnThread()

        var currentPosition = (0, 0)
        var currentHeading = 0

        val visited: Set[Tuple2[Int, Int]] = new HashSet()
        val white: Set[Tuple2[Int, Int]] = new HashSet()
        visited.add(currentPosition)
        white.add(currentPosition)

        while (!computer.isFinished()) {

            input.put(if (white.contains(currentPosition)) 1 else 0)

            val color = output.poll(100, TimeUnit.MILLISECONDS)
            if (color != null) {
                if (color == 1) white.add(currentPosition)
                else white.remove(currentPosition)

                val direction = output.take()
                if (direction == 1) currentHeading = (currentHeading + 1) % headings.size
                else currentHeading = if (currentHeading == 0) 3 else currentHeading - 1

                currentPosition = (currentPosition._1 + headings(currentHeading)._1, currentPosition._2 + headings(currentHeading)._2)
                visited.add(currentPosition)
            }

        }

        def max = white.toArray(new Array[Tuple2[Int, Int]](white.size())).fold((0, 0)) { (a, b) => (Math.max(a._1, b._1), Math.max(a._2, b._2)) }
        def min = white.toArray(new Array[Tuple2[Int, Int]](white.size())).fold((0, 0)) { (a, b) => (Math.min(a._1, b._1), Math.min(a._2, b._2)) }

        def offset = (0 - min._1, 0 - min._2)
        def coords = white.toArray(new Array[Tuple2[Int, Int]](white.size())).map { it => (it._1 + offset._1, it._2 + offset._2) }.toSet
        def maxCoord = coords.fold((0, 0)) { (a, b) => (Math.max(a._1, b._1), Math.max(a._2, b._2)) }

        def hull = (maxCoord._2 to 0 by -1).map { y =>
            (0 to maxCoord._1).map { x => if (coords.contains((x, y))) "*" else " " }.mkString
        }.mkString("\n")

        hull

    }

}
