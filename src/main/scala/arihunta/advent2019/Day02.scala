package arihunta.advent2019

import scala.io.Source
import java.util.Arrays

object Day02 {

    def _01(): Int = {
        solveProgram(Source.fromResource("02").mkString.split(",").map(it => Integer.parseInt(it)), 12, 2)
    }

    def _02(): Int = {

        val program = Source.fromResource("02").mkString.split(",").map(it => Integer.parseInt(it))

        for (i <- 0 to 99) {
            for (j <- 0 to 99) {
                if (solveProgram(program, i, j) == 19690720)
                    return (i * 100) + j
            }
        }

        -1

    }

    def solveProgram(input: Array[Int], noun: Int, verb: Int): Int = {

        val program = input.clone()

        program(1) = noun
        program(2) = verb

        var position = 0

        while (program(position) != 99) {
            if (program(position) == 1) {
                program(program(position + 3)) = program(program(position + 1)) + program(program(position + 2))
            } else if (program(position) == 2) {
                program(program(position + 3)) = program(program(position + 1)) * program(program(position + 2))
            }
            position += 4
        }

        program(0)

    }

}
