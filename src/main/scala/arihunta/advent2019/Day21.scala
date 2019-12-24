package arihunta.advent2019

import scala.io.Source

object Day21 {

    def _01(): Int = {

        val program = Source.fromResource("21").mkString.trim.split(",").map { BigInt(_) }

        val computer = new IntCodeComputer(program)

        val prog = """
NOT C T
NOT B J
OR J T
NOT A J
OR J T
NOT D J
OR D J
AND D J
AND T J
WALK
""".trim() + "\n"

        prog.chars.forEach { computer.input.add(_) }

        computer.runOnThread()

        while (true) {
            val out = computer.output.take
            if (out > 127) return out.intValue()
        }

        -1

    }

    def _02(): Int = {

        val program = Source.fromResource("21").mkString.trim.split(",").map { BigInt(_) }

        val computer = new IntCodeComputer(program)

        val prog = """
NOT C T
NOT B J
OR J T
NOT A J
OR J T
NOT D J
OR D J
AND D J
AND T J
NOT E T
OR E T
AND E T
OR H T
AND T J
RUN
""".trim() + "\n"

    /*
     *    ( ( ( (NOT A) OR (NOT B) ) OR (NOT C) ) AND D ) AND ( E OR H )
     */

        prog.chars.forEach { computer.input.add(_) }

        computer.runOnThread()

        while (true) {
            val out = computer.output.take
            if (out > 127) return out.intValue()
            else print(out.toChar)
        }

        -1

    }

}
