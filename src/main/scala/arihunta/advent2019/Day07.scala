package arihunta.advent2019

import scala.io.Source
import java.util.concurrent.LinkedBlockingQueue
import java.util.Queue
import java.util.Arrays
import java.util.concurrent.BlockingQueue

object Day07 {

    def _01() = {

        val program = Source.fromResource("07").mkString.trim().split(",").map(it => it.toInt)

        generateCombinations(Array(0, 1, 2, 3, 4))
            .map(it => runTest(program, it))
            .max

    }

    def _02() = {

        val program = Source.fromResource("07").mkString.trim().split(",").map(it => it.toInt)

        generateCombinations(Array(5, 6, 7, 8, 9))
            .map(it => runTestFeedbackLoop(program, it))
            .max

    }

    def generateCombinations(input: Array[Int]): Array[Array[Int]] = {
        if (input.size == 1)
            Array(input)
        else {
            input.flatMap(it => {
                val tmp = input.clone().filter(num => num != it)
                generateCombinations(tmp).map(arr => it +: arr)
            })
        }
    }

    private def runTest(program: Array[Int], inputs: Array[Int]): Int = {

        val _in01: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(0)))
        val _out01_in02: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(1)))
        val _out02_in03: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(2)))
        val _out03_in04: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(3)))
        val _out04_in05: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(4)))
        val _out05: BlockingQueue[Int] = new LinkedBlockingQueue()

        _in01.put(0)

        val amp01 = new IntCodeComputer(program.clone(), _in01, _out01_in02)
        val amp02 = new IntCodeComputer(program.clone(), _out01_in02, _out02_in03)
        val amp03 = new IntCodeComputer(program.clone(), _out02_in03, _out03_in04)
        val amp04 = new IntCodeComputer(program.clone(), _out03_in04, _out04_in05)
        val amp05 = new IntCodeComputer(program.clone(), _out04_in05, _out05)

        amp01.runOnThread()
        amp02.runOnThread()
        amp03.runOnThread()
        amp04.runOnThread()
        amp05.runOnThread()

        amp05.output.take()

    }

    private def runTestFeedbackLoop(program: Array[Int], inputs: Array[Int]): Int = {

        val _out05_in01: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(0)))
        val _out01_in02: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(1)))
        val _out02_in03: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(2)))
        val _out03_in04: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(3)))
        val _out04_in05: BlockingQueue[Int] = new LinkedBlockingQueue(Arrays.asList(inputs(4)))

        _out05_in01.put(0)

        val amp01 = new IntCodeComputer(program.clone(), _out05_in01, _out01_in02)
        val amp02 = new IntCodeComputer(program.clone(), _out01_in02, _out02_in03)
        val amp03 = new IntCodeComputer(program.clone(), _out02_in03, _out03_in04)
        val amp04 = new IntCodeComputer(program.clone(), _out03_in04, _out04_in05)
        val amp05 = new IntCodeComputer(program.clone(), _out04_in05, _out05_in01)

        val thread01 = amp01.runOnThread()
        val thread02 = amp02.runOnThread()
        val thread03 = amp03.runOnThread()
        val thread04 = amp04.runOnThread()
        val thread05 = amp05.runOnThread()

        Array(
                thread01,
                thread02,
                thread03,
                thread04,
                thread05,
        ).foreach(it => it.join())

        amp05.output.remove()

    }

}
