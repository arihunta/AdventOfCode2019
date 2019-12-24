package arihunta.advent2019

import scala.io.Source

object Day20 {

    def _01(inputData: String) = {

        val input = Source.fromResource(inputData).mkString
            .split("\n")
            .map { _.chars.mapToObj { chr => Character.valueOf(chr.asInstanceOf[Char]) }.toArray }
            .toArray

        implicit val nodes: Set[(Int, Int)] = input
            .zipWithIndex
            .flatMap {
                case (arr, y) =>
                    arr.zipWithIndex.map {
                        case (sqr, x) =>
                            (x, y) -> sqr.toString()
                    }
            }
            .filter { _._2 == "." }
            .map { _._1 }
            .toSet

        val portals = nodes.flatMap { node =>
            Array(
                node -> ((node._1 - 2, node._2), (node._1 - 1, node._2)),
                node -> ((node._1 + 1, node._2), (node._1 + 2, node._2)),
                node -> ((node._1, node._2 - 2), (node._1, node._2 - 1)),
                node -> ((node._1, node._2 + 1), (node._1, node._2 + 2)))
        }
            .map { mapping => mapping._1 -> (input(mapping._2._1._2)(mapping._2._1._1).toString() + input(mapping._2._2._2)(mapping._2._2._1).toString()) }
            .filter { mapping => mapping._2.matches("[A-Z][A-Z]") }
            .groupBy { _._2 }
            .mapValues { _.map { _._1 }.toArray }

        val portalSteps = portals
            .filter { _._2.size == 2 }
            .flatMap { portal => Array(portal._2(0) -> portal._2(1), portal._2(1) -> portal._2(0)) }
            .toMap

        implicit val graph: Map[(Int, Int), Array[(Int, Int)]] = nodes
            .map {
                coords =>
                    coords ->
                        Array(
                            (coords._1 - 1, coords._2),
                            (coords._1 + 1, coords._2),
                            (coords._1, coords._2 - 1),
                            (coords._1, coords._2 + 1))
                        .filter { nodes.contains(_) }
                        .toArray
            }
            .toMap
            .map { node =>
                node._1 -> (
                    if (portalSteps.contains(node._1)) node._2.+:(portalSteps(node._1))
                    else node._2)
            }

        shortestPath(portals("AA")(0), portals("ZZ")(0))

    }

    def _02(inputData: String) = {

        val input = Source.fromResource(inputData).mkString
            .split("\n")
            .map { _.chars.mapToObj { chr => Character.valueOf(chr.asInstanceOf[Char]) }.toArray }
            .toArray

        val maxX = input(0).length
        val maxY = input.length

        implicit val nodes: Set[(Int, Int)] = input
            .zipWithIndex
            .flatMap {
                case (arr, y) =>
                    arr.zipWithIndex.map {
                        case (sqr, x) =>
                            (x, y) -> sqr.toString()
                    }
            }
            .filter { _._2 == "." }
            .map { _._1 }
            .toSet

        val portals = nodes.flatMap { node =>
            Array(
                node -> ((node._1 - 2, node._2), (node._1 - 1, node._2)),
                node -> ((node._1 + 1, node._2), (node._1 + 2, node._2)),
                node -> ((node._1, node._2 - 2), (node._1, node._2 - 1)),
                node -> ((node._1, node._2 + 1), (node._1, node._2 + 2)))
        }
            .map { mapping => mapping._1 -> (input(mapping._2._1._2)(mapping._2._1._1).toString() + input(mapping._2._2._2)(mapping._2._2._1).toString()) }
            .filter { mapping => mapping._2.matches("[A-Z][A-Z]") }
            .groupBy { _._2 }
            .mapValues { _.map { _._1 }.toArray }

        val portalSteps = portals
            .filter { _._2.size == 2 }
            .flatMap { portal => Array(portal._2(0) -> portal._2(1), portal._2(1) -> portal._2(0)) }
            .toMap

        implicit val stepsUp = portalSteps.filter { step =>
            step._1._1 == 2 ||
                step._1._1 == (maxX - 3) ||
                step._1._2 == 2 ||
                step._1._2 == (maxY - 3)
        }

        implicit val stepsDown: Map[(Int, Int), (Int, Int)] = portalSteps.filter { step => !stepsUp.contains(step._1) }

        implicit val graph: Map[(Int, Int), Array[(Int, Int)]] = nodes
            .map {
                coords =>
                    coords ->
                        Array(
                            (coords._1 - 1, coords._2),
                            (coords._1 + 1, coords._2),
                            (coords._1, coords._2 - 1),
                            (coords._1, coords._2 + 1))
                        .filter { nodes.contains(_) }
                        .toArray
            }
            .toMap

        val graph3d: collection.mutable.Map[(Int, Int, Int), Array[(Int, Int, Int)]] = collection.mutable.Map()
        graph.foreach { node => graph3d.put(_3d(node._1, 0), node._2.map { _3d(_, 0) }) }

        shortestPath3d(_3d(portals("AA")(0), 0), _3d(portals("ZZ")(0), 0), stepsDown, stepsUp)(graph3d, graph)

    }

    def shortestPath(start: (Int, Int), target: (Int, Int))(implicit graph: Map[(Int, Int), Array[(Int, Int)]]): Int = {

        val visited: collection.mutable.Set[(Int, Int)] = collection.mutable.Set()

        val que: collection.mutable.Queue[(Int, (Int, Int))] = collection.mutable.Queue()
        que.enqueue((0, start))

        while (!que.isEmpty) {

            val current = que.dequeue()
            visited.add(current._2)

            if (current._2 == target) return current._1

            graph(current._2)
                .filter { !visited.contains(_) }
                .foreach { node => que.enqueue((current._1 + 1, node)) }

        }

        -1

    }

    def shortestPath3d(
        start: (Int, Int, Int),
        target: (Int, Int, Int),
        stepsDown: Map[(Int, Int), (Int, Int)],
        stepsUp: Map[(Int, Int), (Int, Int)])(implicit
        graph3d: collection.mutable.Map[(Int, Int, Int), Array[(Int, Int, Int)]],
        graph: Map[(Int, Int), Array[(Int, Int)]]): Int = {

        val visited: collection.mutable.Set[(Int, Int, Int)] = collection.mutable.Set()

        val que: collection.mutable.Queue[(Int, (Int, Int, Int))] = collection.mutable.Queue()
        que.enqueue((0, start))

        while (!que.isEmpty) {

            val current = que.dequeue()
            visited.add(current._2)

            val _2dpos = (current._2._1, current._2._2)
            val newLevel = current._2._3 + 1
            if (stepsDown.contains(_2dpos) && !graph3d.contains(_3d(_2dpos, newLevel))) {
                // copy level below
                graph.foreach { node => graph3d.put(_3d(node._1, newLevel), node._2.map { _3d(_, newLevel) }) }
                // glue in steps down
                stepsDown.foreach { step =>
                    val steppingPoint = _3d(step._1, newLevel - 1)
                    graph3d.put(steppingPoint, (graph3d(steppingPoint).:+(_3d(step._2, newLevel))))
                }
                // glue in steps up
                stepsUp.foreach { step =>
                    val steppingPoint = _3d(step._1, newLevel)
                    graph3d.put(steppingPoint, (graph3d(steppingPoint).:+(_3d(step._2, newLevel - 1))))
                }
            }

            if (current._2 == target) {
                return current._1
            }

            graph3d(current._2)
                .filter { !visited.contains(_) }
                .foreach { node => que.enqueue((current._1 + 1, node)) }

        }

        -1

    }

    def _3d(coord: (Int, Int), z: Int): (Int, Int, Int) = {
        (coord._1, coord._2, z)
    }

}
