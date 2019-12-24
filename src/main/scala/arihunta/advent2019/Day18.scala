package arihunta.advent2019

import scala.io.Source

object Day18 {

    def _01(inputData: String) = {

        val input = Source.fromResource(inputData).mkString.trim
            .split("\n")
            .map { _.chars.mapToObj { chr => Character.valueOf(chr.asInstanceOf[Char]) }.toArray }
            .toArray

        implicit val map: Map[(Int, Int), String] = input
            .zipWithIndex
            .flatMap {
                case (arr, y) =>
                    arr.zipWithIndex.map {
                        case (sqr, x) =>
                            (x, y) -> sqr.toString()
                    }
            }
            .filter { _._2 != "#" }
            .toMap

        implicit val graph: Map[(Int, Int), Array[(Int, Int)]] = map
            .map {
                case (coords, value) =>
                    coords -> Array(
                        (coords._1 - 1, coords._2),
                        (coords._1 + 1, coords._2),
                        (coords._1, coords._2 - 1),
                        (coords._1, coords._2 + 1))
                        .filter { map.contains(_) }
                        .toArray
            }

        implicit val landmarks: Map[String, (Int, Int)] = map
            .filter { _._2 != "." }
            .map {
                case (coords, value) =>
                    value -> coords
            }
            .toMap

        implicit val memory: collection.mutable.Map[String, Int] = collection.mutable.Map()

        solveRecursive(
            landmarks("@"),
            landmarks.filterKeys(it => it.matches("[a-z]")).keys.toSet)

    }

    def _02(inputData: String) = {

        val input = Source.fromResource(inputData).mkString.trim
            .split("\n")
            .map { _.chars.mapToObj { chr => Character.valueOf(chr.asInstanceOf[Char]) }.toArray }
            .toArray

        implicit val map: Map[(Int, Int), String] = input
            .zipWithIndex
            .flatMap {
                case (arr, y) =>
                    arr.zipWithIndex.map {
                        case (sqr, x) =>
                            (x, y) -> sqr.toString()
                    }
            }
            .filter { _._2 != "#" }
            .toMap

        implicit val graph: Map[(Int, Int), Array[(Int, Int)]] = map
            .map {
                case (coords, value) =>
                    coords -> Array(
                        (coords._1 - 1, coords._2),
                        (coords._1 + 1, coords._2),
                        (coords._1, coords._2 - 1),
                        (coords._1, coords._2 + 1))
                        .filter { map.contains(_) }
                        .toArray
            }

        implicit val landmarks: Map[String, (Int, Int)] = map
            .filter { _._2 != "." }
            .filter { _._2 != "@" }
            .map {
                case (coords, value) =>
                    value -> coords
            }
            .toMap

        implicit val memory: collection.mutable.Map[String, Int] = collection.mutable.Map()

        solveRecursiveMultiple(
            map.filter { _._2 == "@" }.keys.toSet,
            landmarks.filterKeys(it => it.matches("[a-z]")).keys.toSet)

    }

    def solveRecursive(
        startingPoint: (Int, Int),
        remainingKeys: Set[String])(implicit
        graph: Map[(Int, Int), Array[(Int, Int)]],
        items: Map[(Int, Int), String],
        landmarks: Map[String, (Int, Int)],
        previousSolutions: collection.mutable.Map[String, Int]): Int = {

        // terminating condition -- we've picked up all of the keys
        if (remainingKeys.isEmpty) {
            return 0
        }

        // check for memoized solution
        val solutionKey = startingPoint.toString() + remainingKeys.toArray.sorted.mkString
        if (previousSolutions.contains(solutionKey)) {
            return previousSolutions(solutionKey)
        }

        // generate needed information
        val remainingKeyCoordinates: Set[(Int, Int)] =
            remainingKeys.map { landmarks(_) }

        val remainingDoorCoordinates: Set[(Int, Int)] =
            remainingKeyCoordinates
                .map { items(_).toUpperCase() }
                .filter { landmarks.contains(_) }
                .map { landmarks(_) }
                .toSet

        // solve sub-problem from here
        val solution: Int =
            shortestPaths(startingPoint, remainingDoorCoordinates)
                .filter { nextNode => remainingKeyCoordinates.contains(nextNode._1) }
                .map { nextNode =>
                    solveRecursive(
                        nextNode._1,
                        remainingKeyCoordinates.filter { _ != nextNode._1 }.map { items(_) }) + nextNode._2
                }
                .min

        previousSolutions.put(solutionKey, solution)

        solution

    }

    def solveRecursiveMultiple(
        startingPoints: Set[(Int, Int)],
        remainingKeys: Set[String])(implicit
        graph: Map[(Int, Int), Array[(Int, Int)]],
        items: Map[(Int, Int), String],
        landmarks: Map[String, (Int, Int)],
        previousSolutions: collection.mutable.Map[String, Int]): Int = {

        // terminating condition -- we've picked up all of the keys
        if (remainingKeys.isEmpty) {
            return 0
        }

        // check for memoized solution
        val solutionKey = startingPoints.toArray.sorted.mkString + ":" + remainingKeys.toArray.sorted.mkString
        if (previousSolutions.contains(solutionKey)) {
            return previousSolutions(solutionKey)
        }

        // generate needed information
        val remainingKeyCoordinates: Set[(Int, Int)] =
            remainingKeys.map { landmarks(_) }

        val remainingDoorCoordinates: Set[(Int, Int)] =
            remainingKeyCoordinates
                .map { items(_).toUpperCase() }
                .filter { landmarks.contains(_) }
                .map { landmarks(_) }
                .toSet

        // solve sub-problem from here
        val solution: Int =
            startingPoints
                .map { start =>
                    val solutionFromHere = shortestPaths(start, remainingDoorCoordinates)
                        .filter { nextNode => remainingKeyCoordinates.contains(nextNode._1) }
                        .map { nextNode =>
                            solveRecursiveMultiple(
                                startingPoints.filter { _ != start } + nextNode._1,
                                remainingKeyCoordinates.filter { _ != nextNode._1 }.map { items(_) }) + nextNode._2
                        }
                    if (solutionFromHere.isEmpty) -1
                    else solutionFromHere.min
                }
                .filter { _ != -1 }
                .min

        previousSolutions.put(solutionKey, solution)

        solution

    }

    def shortestPaths(
        start: (Int, Int),
        remainingDoors: Set[(Int, Int)])(implicit graph: Map[(Int, Int), Array[(Int, Int)]]): Map[(Int, Int), Int] = {

        val visited: collection.mutable.Map[(Int, Int), Int] = collection.mutable.Map()

        val que: collection.mutable.Queue[(Int, (Int, Int))] = collection.mutable.Queue()
        que.enqueue((0, start))

        while (!que.isEmpty) {

            val current = que.dequeue()
            visited.put(current._2, current._1)

            graph(current._2)
                .filter { !visited.contains(_) }
                .filterNot { remainingDoors.contains(_) }
                .foreach { node => que.enqueue((current._1 + 1, node)) }

        }

        visited.filter { it => it._1 != start }.toMap

    }

}
