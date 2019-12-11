package arihunta.advent2019

import org.scalatest.FunSuite

class Day09Test extends FunSuite {

    test("Day 09-01") {
        assert(Day09._01() == BigInt("2932210790"))
    }

    test("Day 09-02") {
        assert(Day09._02() == 73144)
    }

}
