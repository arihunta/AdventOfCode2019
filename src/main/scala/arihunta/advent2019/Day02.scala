package arihunta.advent2019

import scala.io.Source
import java.util.Arrays

object Day02 {

    def _01(): Int = {
        val program = Source.fromResource("02").mkString.split(",").map(it => Integer.parseInt(it))
        program(1) = 12
        program(2) = 2
        IntCodeComputer.run(program, 0)
        program(0)
    }

    def _02(): Int = {

        val initialProgram = Source.fromResource("02").mkString.split(",").map(it => Integer.parseInt(it))

        for (i <- 0 to 99) {
            for (j <- 0 to 99) {
                val program = initialProgram.clone()
                program(1) = i
                program(2) = j
                IntCodeComputer.run(program, 0)
                if (program(0) == 19690720)
                    return (i * 100) + j
            }
        }

        -1

    }

}
