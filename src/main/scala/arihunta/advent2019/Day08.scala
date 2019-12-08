package arihunta.advent2019

import scala.io.Source

object Day08 {

    final def IMAGE_WIDTH = 25
    final def IMAGE_HEIGHT = 6

    def _01() = {

        val input = Source.fromResource("08").mkString
        val layerSize = IMAGE_HEIGHT * IMAGE_WIDTH

        val layers = (1 to (input.size / layerSize))
                .map(layerNumber => {
                    (layerNumber - 1, input.substring((layerNumber - 1) * layerSize, layerNumber * layerSize))
                })
                .toMap

        val layerInfo = layers
                .map(it => (
                        it._2.replaceAll("[^0]", "").size,
                        it._2.replaceAll("[^1]", "").size,
                        it._2.replaceAll("[^2]", "").size
                ))
                .min

        layerInfo._2 * layerInfo._3

    }

    def _02() = {

        val input = Source.fromResource("08").mkString
        val layerSize = IMAGE_HEIGHT * IMAGE_WIDTH

        val layers = (1 to (input.size / layerSize))
                .map(layerNumber =>
                    input.substring((layerNumber - 1) * layerSize, layerNumber * layerSize)
                )

        val combinedImage = (0 until (IMAGE_HEIGHT * IMAGE_WIDTH))
                .map(pixel => layers.map(layer => layer(pixel)).mkString)
                .map(line => line.replaceAll("2", "")(0))
                .mkString.replaceAll("0", " ").replaceAll("1", "\\*")

        (0 until IMAGE_HEIGHT).map(it => combinedImage.substring(it * IMAGE_WIDTH, (it + 1) * IMAGE_WIDTH)).mkString("\n")

    }

}
