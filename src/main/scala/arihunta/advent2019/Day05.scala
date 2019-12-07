package arihunta.advent2019

import scala.io.Source
import java.util.LinkedList
import java.util.Queue
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.LinkedBlockingQueue

object Day05 {

    def _01(): Int = {
        val program = Source.fromResource("05").mkString.trim().split(",").map(it => it.toInt)
        val input: BlockingQueue[Int] = new LinkedBlockingQueue
        input.add(1)
        val computer = new IntCodeComputer(program, input)
        computer.run()
        computer.output.toArray()(computer.output.size() - 1).asInstanceOf[Int]
    }

    def _02(): Int = {
        val program = Source.fromResource("05").mkString.trim().split(",").map(it => it.toInt)
        val input: BlockingQueue[Int] = new LinkedBlockingQueue
        input.add(5)
        val computer = new IntCodeComputer(program, input)
        computer.run()
        computer.output.toArray()(computer.output.size() - 1).asInstanceOf[Int]
    }

}
