package arihunta.advent2019

import scala.io.Source

object Day16 {

    val pattern = Array(0, 1, 0, -1)

    def _01() = {

        val signal = Source.fromResource("16").mkString.trim().chars().map { _ - 48 }.toArray()

        (1 to 100).foreach { _ =>
            val tmp: Array[Int] = Array.ofDim(signal.length)
            var dst_idx = 0
            while (dst_idx < signal.length) {
                var iter = signal.iterator.drop(dst_idx)
                while (iter.hasNext) {
                    tmp(dst_idx) += iter.take(dst_idx + 1).sum
                    iter = iter.drop(2 * dst_idx + 2)
                    tmp(dst_idx) = -tmp(dst_idx)
                }
                tmp(dst_idx) = math.abs(tmp(dst_idx) % 10)
                dst_idx += 1
            }
            Array.copy(tmp, 0, signal, 0, tmp.length)
        }

        signal.take(8).mkString

    }

    def _02() = {

        val input = Source.fromResource("16").mkString.trim().chars().map { _ - 48 }.toArray()
        val offset = input.take(7).mkString.toInt

        val tmp: Array[Int] = Array.ofDim(input.size * 10000)
        (0 until 10000).foreach { iter => Array.copy(input, 0, tmp, iter * input.length, input.length) }

        val signalLength = tmp.length
        val signal = tmp.drop(offset)

        (1 to 100).foreach { iter =>
            val tmp: Array[Int] = Array.ofDim(signal.length)
            var dst_idx = offset
            while (dst_idx < signalLength) {
//                println("column: " + dst_idx)
                var sum_idx = 0 + (dst_idx)
                while (sum_idx < signalLength) {
                    var pat_idx = 0
                    while (sum_idx < signalLength && pat_idx <= dst_idx) {
                        tmp(dst_idx - offset) += signal(sum_idx - offset)
                        sum_idx += 1
                        pat_idx += 1
                    }
                    sum_idx += dst_idx + 1
                    tmp(dst_idx - offset) = -tmp(dst_idx - offset)
                }
                tmp(dst_idx - offset) = math.abs(tmp(dst_idx - offset) % 10)
                dst_idx += 1
            }
            Array.copy(tmp, 0, signal, 0, tmp.length)
//            println(signal.mkString)
        }

        signal.take(8).mkString

    }

}
