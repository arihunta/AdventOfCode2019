package arihunta.advent2019

import org.scalatest.FunSuite

class Day08Test extends FunSuite {

    test("Day 08-01") {
        assert(Day08._01() == 2440)
    }

    test("Day 08-02") {
        assert(Day08._02() ==
            " **  ****  **    **  **  " + "\n" +
            "*  *    * *  *    * *  * " + "\n" +
            "*  *   *  *       * *    " + "\n" +
            "****  *   *       * *    " + "\n" +
            "*  * *    *  * *  * *  * " + "\n" +
            "*  * ****  **   **   **  ")
    }

}
