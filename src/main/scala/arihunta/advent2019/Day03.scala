package arihunta.advent2019

import scala.io.Source
import java.util.Arrays
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.math.min
import scala.collection.mutable.ArrayBuffer

object Day03 {

    def _01(): Int = {

        val paths = Source.fromResource("03").mkString.trim().split("\n")
            .map(path => {
                path.split(",").flatMap(instruction => {
                    val number = Integer.parseInt(instruction.substring(1))
                    instruction(0) match {
                        case 'R' => (1 to number).map(it => (0, 1))
                        case 'L' => (1 to number).map(it => (0, -1))
                        case 'U' => (1 to number).map(it => (1, 0))
                        case 'D' => (1 to number).map(it => (-1, 0))
                        case _ => Stream.Empty
                    }
                })
            })

        val path1: scala.collection.mutable.Set[(Int, Int)] = Set()
        val path2: scala.collection.mutable.Set[(Int, Int)] = Set()

        var coords = (0, 0)
        for (step <- paths(0)) {
            coords = (coords._1 + step._1, coords._2 + step._2)
            path1 += coords
        }
        coords = (0, 0)
        for (step <- paths(1)) {
            coords = (coords._1 + step._1, coords._2 + step._2)
            path2 += coords
        }
        val intersections = path1.intersect(path2)

        val distances = intersections.map(it => it._1.abs + it._2.abs).toList.sorted

        distances(0)

    }

    def _02(): Int = {

        val paths = Source.fromResource("03").mkString.trim().split("\n")
            .map(path => {
                path.split(",").flatMap(instruction => {
                    val number = Integer.parseInt(instruction.substring(1))
                    instruction(0) match {
                        case 'R' => (1 to number).map(it => (0, 1))
                        case 'L' => (1 to number).map(it => (0, -1))
                        case 'U' => (1 to number).map(it => (1, 0))
                        case 'D' => (1 to number).map(it => (-1, 0))
                        case _ => Stream.Empty
                    }
                })
            })

        val points1: scala.collection.mutable.Set[(Int, Int)] = Set()
        val points2: scala.collection.mutable.Set[(Int, Int)] = Set()

        val path1: ArrayBuffer[(Int, Int)] = ArrayBuffer()
        val path2: ArrayBuffer[(Int, Int)] = ArrayBuffer()

        var coords = (0, 0)
        for (step <- paths(0)) {
            coords = (coords._1 + step._1, coords._2 + step._2)
            points1 += coords
            path1 += coords
        }
        coords = (0, 0)
        for (step <- paths(1)) {
            coords = (coords._1 + step._1, coords._2 + step._2)
            points2 += coords
            path2 += coords
        }
        val intersections = points1.intersect(points2)

        val distances = intersections
                .toList
                .map(it => path1.indexOf(it) + path2.indexOf(it) + 2)
                .sorted

        return distances(0)

    }

}
