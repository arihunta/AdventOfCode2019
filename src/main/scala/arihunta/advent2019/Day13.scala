package arihunta.advent2019

import scala.io.Source
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

object Day13 {

    def _01(): Int = {

        val program = Source.fromResource("13").mkString.trim.split(",").map(it => BigInt(it))

        val output: BlockingQueue[BigInt] = new LinkedBlockingQueue

        val computer = new IntCodeComputer(program, output = output)

        val blocks: Set[Tuple2[Int, Int]] = new HashSet()

        computer.runOnThread()

        while (true) {

            val x = output.poll(100, TimeUnit.MILLISECONDS)

            if (x == null) return blocks.size

            val coord = (x.intValue(), output.take().intValue())
            val item = output.take().intValue()

            if (item == 2) blocks.add(coord)
            else blocks.remove(coord)

        }

        -1

    }

    def _02(): Int = {

        val program = Source.fromResource("13").mkString.trim.split(",").map(it => BigInt(it))

        program(0) = 2

        val input: BlockingQueue[BigInt] = new LinkedBlockingQueue
        val output: BlockingQueue[BigInt] = new LinkedBlockingQueue

        val computer = new IntCodeComputer(program, input, output)

        val screen: Array[Array[Char]] = (1 to 45).map(it => (1 to 20).map(thing => ' ').toArray).toArray

        var score = 0

        var ball = (-1, -1)
        var paddle = (-1, -1)

        computer.runOnThread()

        val print = false

        while (true) {

            val x = output.poll(10, TimeUnit.MILLISECONDS)

            if (x == null) {
                if (computer.isFinished()) return score
                input.add(
                    if (ball._1 < paddle._1) -1
                    else if (ball._1 > paddle._1) 1
                    else 0
                )
            }
            else {

                val coord = (x.intValue(), output.take().intValue())
                val item = output.take().intValue()

                if (coord == (-1, 0)) { score = item }
                else if (item == 0) { screen(coord._1)(coord._2) = ' ' }
                else if (item == 1) { screen(coord._1)(coord._2) = '#' }
                else if (item == 2) { screen(coord._1)(coord._2) = 'H' }
                else if (item == 3) { screen(coord._1)(coord._2) = '|'; paddle = coord }
                else if (item == 4) { screen(coord._1)(coord._2) = '*'; ball = coord }

                if (print) {
                    println("Score: " + score)
                    println (screen.map(it => it.mkString).mkString("\n"))
                }

            }

        }

        -1

    }

}
