package arihunta.advent2019

import java.util.Queue
import java.util.LinkedList
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.LinkedBlockingQueue

class IntCodeComputer(
        val program: Array[Int],
        val input: BlockingQueue[Int] = new LinkedBlockingQueue[Int],
        val output: BlockingQueue[Int] = new LinkedBlockingQueue[Int],
    ) {

    private var programCounter = 0

    def run() = {

        while (program(programCounter) != 99) {

            val instruction = "%05d".format(program(programCounter))

            val opcode = instruction.substring(3, 5).toInt
            val param3mode = Integer.parseInt(instruction(0).toString())
            val param2mode = Integer.parseInt(instruction(1).toString())
            val param1mode = Integer.parseInt(instruction(2).toString())

            opcode match {
                case 1 => {
                    val param1 = param(program, programCounter, param1mode, 1)
                    val param2 = param(program, programCounter, param2mode, 2)
                    program(program(programCounter + 3)) = param1 + param2
                    programCounter += 4
                }
                case 2 => {
                    val param1 = param(program, programCounter, param1mode, 1)
                    val param2 = param(program, programCounter, param2mode, 2)
                    program(program(programCounter + 3)) = param1 * param2
                    programCounter += 4
                }
                case 3 => {
                    program(program(programCounter + 1)) = input.take()
                    programCounter += 2
                }
                case 4 => {
                    output.put(param(program, programCounter, param1mode, 1))
                    programCounter += 2
                }
                case 5 => {
                    programCounter =
                        if (param(program, programCounter, param1mode, 1) != 0)
                            param(program, programCounter, param2mode, 2)
                        else programCounter + 3
                }
                case 6 => {
                    programCounter =
                        if (param(program, programCounter, param1mode, 1) == 0)
                            param(program, programCounter, param2mode, 2)
                        else programCounter + 3
                }
                case 7 => {
                    program(program(programCounter + 3)) =
                        if (param(program, programCounter, param1mode, 1) < param(program, programCounter, param2mode, 2)) 1
                        else 0
                    programCounter += 4
                }
                case 8 => {
                    program(program(programCounter + 3)) =
                        if (param(program, programCounter, param1mode, 1) == param(program, programCounter, param2mode, 2)) 1
                        else 0
                    programCounter += 4
                }
            }
        }

    }

    def runOnThread(): Thread = {
        val thread = new Thread(() => run())
        thread.start()
        thread
    }

    private def param(program: Array[Int], position: Int, mode: Int, param: Int): Int = {
        if (mode == 1) program(position + param) else program(program(position + param))
    }

}
