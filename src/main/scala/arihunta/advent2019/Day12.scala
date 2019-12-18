package arihunta.advent2019

import scala.io.Source

object Day12 {

    val rx = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r

    def _01() = {

        // read initial positions from input
        val positions: Array[(Int, Int, Int)] = Source.fromResource("12").mkString.trim()
            .split("\n")
            .map { it =>
                def ans = rx.findAllIn(it).matchData.map { it =>
                    (it.group(1).toInt, it.group(2).toInt, it.group(3).toInt)
                }.toArray
                ans(0)
            }.toArray

        // initialise velocities to zero
        val velocities: Array[(Int, Int, Int)] = Array(
            (0, 0, 0),
            (0, 0, 0),
            (0, 0, 0),
            (0, 0, 0),
        )

        // run iterations
        (1 to 1000).foreach { step =>

            // calculate velocities from positions
            (0 to 3).foreach { moon =>
                (0 to 3).filter(_ != moon).foreach { otherMoon =>
                    velocities(moon) = (
                            velocities(moon)._1 + (if (positions(moon)._1 > positions(otherMoon)._1) -1 else if (positions(moon)._1 < positions(otherMoon)._1) 1 else 0),
                            velocities(moon)._2 + (if (positions(moon)._2 > positions(otherMoon)._2) -1 else if (positions(moon)._2 < positions(otherMoon)._2) 1 else 0),
                            velocities(moon)._3 + (if (positions(moon)._3 > positions(otherMoon)._3) -1 else if (positions(moon)._3 < positions(otherMoon)._3) 1 else 0),
                    )
                }
            }

            // calculate positions from velocities
            (0 to 3).foreach { moon =>
                positions(moon) = (
                        positions(moon)._1 + velocities(moon)._1,
                        positions(moon)._2 + velocities(moon)._2,
                        positions(moon)._3 + velocities(moon)._3,
                )
            }

        }

        // calculate final energy
        (0 to 3).map { moon =>
                (positions(moon)._1.abs + positions(moon)._2.abs + positions(moon)._3.abs) *
                (velocities(moon)._1.abs + velocities(moon)._2.abs + velocities(moon)._3.abs)
        }.sum

    }

    def _02(): Long = {

        // read initial positions from input
        val positions: Array[Array[Int]] = Source.fromResource("12").mkString.trim()
            .split("\n")
            .map { it =>
                def ans = rx.findAllIn(it).matchData.map { it =>
                    Array(it.group(1).toInt, it.group(2).toInt, it.group(3).toInt)
                }.toArray
                ans(0)
            }.toArray

        // read initial positions from input
        val initialPositions: Array[Array[Int]] = Source.fromResource("12").mkString.trim()
            .split("\n")
            .map { it =>
                def ans = rx.findAllIn(it).matchData.map { it =>
                    Array(it.group(1).toInt, it.group(2).toInt, it.group(3).toInt)
                }.toArray
                ans(0)
            }.toArray

        // initialise velocities to zero
        val velocities: Array[Array[Int]] = Array(
            Array(0, 0, 0),
            Array(0, 0, 0),
            Array(0, 0, 0),
            Array(0, 0, 0),
        )

        val periods: Array[Long] = Array(1, 1, 1)

        (0 to 2).foreach { axis =>

            var step = 0;

            // run iterations
            do {

                step = step + 1

                // calculate velocities from positions
                (0 to 3).foreach { moon =>
                    (0 to 3).filter( _ != moon).foreach { otherMoon =>
                        velocities(moon)(axis) =
                                velocities(moon)(axis) + (
                                        if (positions(moon)(axis) > positions(otherMoon)(axis)) -1
                                        else if (positions(moon)(axis) < positions(otherMoon)(axis)) 1
                                        else 0
                                )
                    }
                }

                // calculate positions from velocities
                (0 to 3).foreach { moon =>
                    positions(moon)(axis) =
                            positions(moon)(axis) + velocities(moon)(axis)
                }

            } while (!velocities.forall(_(axis) == 0) || !(0 to 3).forall(it => positions(it)(axis) == initialPositions(it)(axis)))

            periods(axis) = step

        }

        val tmpLcm = ((periods(1) * periods(2)) / greatestCommonFactor(periods(1), periods(2)))
        (periods(0) * tmpLcm) / greatestCommonFactor(periods(0), tmpLcm)

    }

    private def greatestCommonFactor(a: Long, b: Long): Long = {
        if (a == 0) b
        else if (b == 0) a
        else if (a > b) greatestCommonFactor(a - b, b)
        else greatestCommonFactor(a, b - a)
    }

}
