package arihunta.advent2019

import scala.io.Source
import java.util.Arrays

object Day01 {

    def _01(): Int = {
        Source.fromResource("01").mkString
            .split("\n")
            .map(it => Integer.parseInt(it))
            .map(it => it / 3)
            .map(it => it - 2)
            .sum
    }

    def _02(): Int = {
        Source.fromResource("01").mkString
            .split("\n")
            .map(it => Integer.parseInt(it))
            .map(it => {
                var sum = 0
                var mass = (it / 3) - 2
                while (mass >= 0) {
                    sum += mass
                    mass = (mass / 3) - 2
                }
                sum
            })
            .sum
    }

}
