package arihunta.advent2019

import java.util.Queue
import java.util.LinkedList
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt
import org.apache.logging.log4j.LogManager

class IntCodeComputer(
        private val programIn: Array[BigInt],
        val input: BlockingQueue[BigInt] = new LinkedBlockingQueue[BigInt],
        val output: BlockingQueue[BigInt] = new LinkedBlockingQueue[BigInt],
        val debug: Boolean = false,
    ) {

    val logger = LogManager.getLogger(classOf[IntCodeComputer]);

    val program : ArrayBuffer[BigInt] = ArrayBuffer(programIn: _*)
    private var programCounter: BigInt = 0
    private var relativeBase: BigInt = 0
    private var finished: Boolean = false

    def run() = {

        while (prog(programCounter) != 99) {

            val instruction = "%05d".format(prog(programCounter))

            val opcode = instruction.substring(3, 5).toInt
            val param3mode = Integer.parseInt(instruction(0).toString())
            val param2mode = Integer.parseInt(instruction(1).toString())
            val param1mode = Integer.parseInt(instruction(2).toString())

            if (debug) logger.debug("OPCODE: " + opcode)

            opcode match {
                case 1 => {
                    val param1 = param(param1mode, 1)
                    val param2 = param(param2mode, 2)
                    set(param3mode, 3, param1 + param2)
                    programCounter += 4
                }
                case 2 => {
                    val param1 = param(param1mode, 1)
                    val param2 = param(param2mode, 2)
                    set(param3mode, 3, param1 * param2)
                    programCounter += 4
                }
                case 3 => {
                    val inVal = input.take()
                    if (debug) logger.debug("input: " + inVal)
                    set(param1mode, 1, inVal)
                    programCounter += 2
                }
                case 4 => {
                    val outVal = param(param1mode, 1)
                    if (debug) logger.debug("output: " + outVal)
                    output.put(outVal)
                    programCounter += 2
                }
                case 5 => {
                    programCounter =
                        if (param(param1mode, 1) != 0)
                            param(param2mode, 2)
                        else programCounter + 3
                }
                case 6 => {
                    programCounter =
                        if (param(param1mode, 1) == 0)
                            param(param2mode, 2)
                        else programCounter + 3
                }
                case 7 => {
                    set(param3mode, 3,
                        if (param(param1mode, 1) < param(param2mode, 2)) 1
                        else 0
                    )
                    programCounter += 4
                }
                case 8 => {
                    set(param3mode, 3,
                        if (param(param1mode, 1) == param(param2mode, 2)) 1
                        else 0
                    )
                    programCounter += 4
                }
                case 9 => {
                    relativeBase += param(param1mode, 1)
                    programCounter += 2
                }
            }
        }

        if (debug) logger.debug("Exiting: " + prog(programCounter))

        finished = true

    }

    def runOnThread(): Thread = {
        val thread = new Thread(() => run())
        thread.start()
        thread
    }

    def isFinished(): Boolean = finished

    private def param(mode: Int, param: BigInt): BigInt = {
        if (mode == 0) prog(prog(programCounter + param))
        else if (mode == 1) prog(programCounter + param)
        else if (mode == 2) prog(relativeBase + prog(programCounter + param))
        else -1
    }

    private def set(mode: Int, param: BigInt, value: BigInt) = {
        val idx: BigInt =
            if (mode == 0) prog(programCounter + param)
            else if (mode == 1) programCounter + param
            else if (mode == 2) relativeBase + prog(programCounter + param)
            else throw new IllegalStateException("Invalid parameter mode")
        while (idx >= program.size) {
            program += 0
        }
        program(idx.toInt) = value
    }

    private def prog(idx: BigInt): BigInt = {
        while (idx >= program.size) {
            program += 0
        }
        program(idx.toInt)
    }

}
