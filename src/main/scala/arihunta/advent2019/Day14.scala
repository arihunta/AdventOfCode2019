package arihunta.advent2019

import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Queue
import scala.collection.mutable.Map

object Day14 {

    def _01() = {

        val equations = Source.fromResource("14").mkString.trim.split("\n")
            .map { _.split(" => ") }
            .map { eqn => eqn(1) -> eqn(0) }
            .map { eqn => stringToChemical(eqn._1) -> eqn._2.split(", ").map { stringToChemical(_) } }
            .map { eqn => eqn._1._2 -> eqn }
            .toMap

        val que: Queue[(Int, String)] = Queue()
        val surplus: Map[String, Int] = Map()

        var ore = 0

        que.enqueue((1, "FUEL"))

        while (!que.isEmpty) {

            val current = que.dequeue()

            if (current._2 == "ORE") ore = ore + current._1
            else {

                val eqn = equations(current._2)
                val desiredQuantity = Math.max(0, current._1 - surplus.getOrElse(current._2, 0))

                val remainder = desiredQuantity % eqn._1._1
                val numReactions = (desiredQuantity / eqn._1._1) + (if (remainder == 0) 0 else 1)

                if (numReactions > 0)
                    eqn._2
                        .map { chem => (chem._1 * numReactions, chem._2) }
                        .foreach { que.enqueue(_) }

                if (remainder > 0) {
                    surplus.put(current._2, eqn._1._1 - remainder)
                } else if (current._1 - surplus.getOrElse(current._2, 0) < 0) {
                    surplus.put(current._2, math.abs(current._1 - surplus.getOrElse(current._2, 0)))
                } else surplus.remove(current._2)

            }

        }

        ore

    }

    def _02() = {

        val equations = Source.fromResource("14").mkString.trim.split("\n")
            .map { _.split(" => ") }
            .map { eqn => eqn(1) -> eqn(0) }
            .map { eqn => stringToChemical(eqn._1) -> eqn._2.split(", ").map { stringToChemical(_) } }
            .map { eqn => eqn._1._2 -> eqn }
            .toMap

        val que: Queue[(Int, String)] = Queue()
        val surplus: Map[String, Int] = Map()

        var ore: Long = 0
        var fuelCount = 0

        while (ore < 1000000000000L) {

            que.enqueue((1, "FUEL"))
            fuelCount = fuelCount + 1

            while (!que.isEmpty) {

                val current = que.dequeue()

                if (current._2 == "ORE") ore = ore + current._1
                else {

                    val eqn = equations(current._2)
                    val desiredQuantity = Math.max(0, current._1 - surplus.getOrElse(current._2, 0))

                    val remainder = desiredQuantity % eqn._1._1
                    val numReactions = (desiredQuantity / eqn._1._1) + (if (remainder == 0) 0 else 1)

                    if (numReactions > 0)
                        eqn._2
                            .map { chem => (chem._1 * numReactions, chem._2) }
                            .foreach { que.enqueue(_) }

                    if (remainder > 0) {
                        surplus.put(current._2, eqn._1._1 - remainder)
                    } else if (current._1 - surplus.getOrElse(current._2, 0) < 0) {
                        surplus.put(current._2, math.abs(current._1 - surplus.getOrElse(current._2, 0)))
                    } else surplus.remove(current._2)

                }

            }

        }

        fuelCount - 1

    }

    private def stringToChemical(str: String): (Int, String) = {
        val split = str.trim.split(" ")
        (split(0).toInt, split(1))
    }

}
