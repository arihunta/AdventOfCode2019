package arihunta.advent2019

import org.scalatest.FunSuite

class Day11Test extends FunSuite {

    test("Day 11-01") {
        assert(Day11._01() == 2141)
    }

    test("Day 11-02") {
        assert(Day11._02() ==
            " ***  ***    **  **  **** **** *  * ****" + "\n" +
            " *  * *  *    * *  * *       * * *  *   " + "\n" +
            " *  * *  *    * *    ***    *  **   *** " + "\n" +
            " ***  ***     * *    *     *   * *  *   " + "\n" +
            " * *  *    *  * *  * *    *    * *  *   " + "\n" +
            " *  * *     **   **  *    **** *  * *   ")
    }

}
