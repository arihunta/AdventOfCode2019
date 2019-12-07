package arihunta.advent2019

import scala.io.Source

object Day05 {
    
    def _01(): Int = {
        val program = Source.fromResource("05").mkString.trim().split(",").map(it => Integer.parseInt(it))
        IntCodeComputer.run(program, 1)
    }
    
    def _02(): Int = {
        val program = Source.fromResource("05").mkString.trim().split(",").map(it => Integer.parseInt(it))
        IntCodeComputer.run(program, 5)
    }
    
}
