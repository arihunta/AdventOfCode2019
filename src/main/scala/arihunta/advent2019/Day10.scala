package arihunta.advent2019

import scala.io.Source
import java.util.stream.IntStream

object Day10 {

    def _01() = {

        def asteroids = Source.fromResource("10").mkString.trim
            .split("\n")
            .map {
                _.toCharArray()
                    .zipWithIndex
                    .filter { _._1 == '#' }
                    .map { _._2 }
            }
            .zipWithIndex
            .flatMap { row => row._1.map { (_, row._2) } }
            .toList

        val answer = asteroids
            .map { asteroid =>
                val numberOfVisibleAsteroids = asteroids
                    .filter { _ != asteroid }
                    .map { other => (other._1 - asteroid._1, other._2 - asteroid._2) }
                    .map { it =>
                        val gcf: Int = greatestCommonFactor(it._1.abs, it._2.abs)
                        (it._1 / gcf, it._2 / gcf)
                    }
                    .toSet.size
                (numberOfVisibleAsteroids, asteroid)
            }
            .max

        println("Coordinates: " + answer._2)

        answer._1

    }

    def _02() = {

        def coordinates = (23, 29)

        def asteroids = Source.fromResource("10").mkString.trim
            .split("\n")
            .map {
                _.toCharArray()
                    .zipWithIndex
                    .filter { _._1 == '#' }
                    .map { _._2 }
            }
            .zipWithIndex
            .flatMap { row => row._1.map { (_, row._2) } }
            .filter { _ != coordinates }
            .toList

        val sightLines = asteroids
            .map { other => (other, (other._1 - coordinates._1, coordinates._2 - other._2)) }
            .map { it =>
                val gcf: Int = greatestCommonFactor(it._2._1.abs, it._2._2.abs)
                (it._1, (it._2._1 / gcf, it._2._2 / gcf))
            }
            .groupBy { _._2 }
            .mapValues { _.map(_._1).sortBy { it => Math.hypot(coordinates._1 - it._1, coordinates._2 - it._2) } }

        val orderedSightLines = sightLines.keys.toList.sortBy { it =>
            val angle = 90 - Math.toDegrees(Math.atan2(it._2, it._1))
            if (angle < 0) angle + 360
            else angle
        }

        val twoHundredth = sightLines(orderedSightLines(199))
        twoHundredth(0)._1 * 100 + twoHundredth(0)._2

    }

    def greatestCommonFactor(a: Int, b: Int): Int = {
        if (a == 0) b
        else if (b == 0) a
        else if (a > b) greatestCommonFactor(a - b, b)
        else greatestCommonFactor(a, b - a)
    }

}
