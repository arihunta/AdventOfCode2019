package arihunta.advent2019

import scala.io.Source
import scala.collection.mutable.Queue

object Day06 {

    def _01() = {

        val orbits = Source.fromResource("06").mkString.trim()
                .split("\n")
                .map(it => (it.split("\\)")))
                .map(it => it(1) -> it(0))
                .toMap

        orbits.keys.toList.map(it => countOrbits(it, orbits)).sum

    }

    def _02() = {

        val orbits = Source.fromResource("06").mkString.trim()
                .split("\n")
                .map(it => (it.split("\\)")))
                .map(it => it(1) -> it(0))
                .toMap
                
        val youDistances = buildDistances("YOU", 0, orbits)
        val santaDistances = buildDistances("SAN", 0, orbits)

        val intersections = youDistances.keySet.intersect(santaDistances.keySet).toList
                .map(it => youDistances(it) + santaDistances(it))
                .sorted
                
        intersections(0) - 2
                
    }

    def countOrbits(key: String, map: Map[String, String]): Int =
        if (map.keySet.contains(key)) 1 + countOrbits(map(key), map)
        else 0

    def buildDistances(key: String, count: Int, orbits: Map[String, String]): Map[String, Int] = {
        if (orbits.contains(key)) {
            buildDistances(orbits(key), count + 1, orbits) + (key->count)
        }
        else List((key->count)).toMap
    }

}
