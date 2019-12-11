package arihunta.advent2019

import scala.io.Source
import java.util.stream.IntStream
import scala.collection.mutable.ArrayBuffer
import java.util.ArrayList
import java.util.stream.Collectors
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue

object Day09 {

    def _01(): BigInt = {
        val program = Source.fromResource("09").mkString.trim.split(",").map(it => BigInt(it))

        val input: BlockingQueue[BigInt] = new LinkedBlockingQueue
        input.add(1)

        val computer = new IntCodeComputer(program, input)
        computer.run()

        computer.output.take()
    }

    def _02(): BigInt = {
        val program = Source.fromResource("09").mkString.trim.split(",").map(it => BigInt(it))

        val input: BlockingQueue[BigInt] = new LinkedBlockingQueue
        input.add(2)

        val computer = new IntCodeComputer(program, input)
        computer.run()

        computer.output.take()
    }

}
