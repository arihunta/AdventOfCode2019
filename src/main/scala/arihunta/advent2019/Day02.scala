package arihunta.advent2019

import scala.io.Source
import java.util.Arrays
import java.util.LinkedList

object Day02 {

    def _01(): Int = {
        val program = Source.fromResource("02").mkString.split(",").map(it => BigInt(it))
        program(1) = 12
        program(2) = 2
        val computer = new IntCodeComputer(program)
        computer.run()
        computer.program(0).toInt
    }

    def _02(): Int = {

        val initialProgram = Source.fromResource("02").mkString.split(",").map(it => BigInt(it))

        for (i <- 0 to 99) {
            for (j <- 0 to 99) {
                val program = initialProgram.clone()
                program(1) = i
                program(2) = j
                val computer = new IntCodeComputer(program)
                computer.run()
                if (computer.program(0) == 19690720)
                    return (i * 100) + j
            }
        }

        -1

    }

}
