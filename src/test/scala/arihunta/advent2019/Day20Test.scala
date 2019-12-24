package arihunta.advent2019

import org.scalatest.FunSuite

class Day20Test extends FunSuite {

    test("Day 20-01-00") {
        assert(Day20._01("20-00") == 23)
    }

    test("Day 20-01-01") {
        assert(Day20._01("20-01") == 58)
    }

    test("Day 20-01") {
        assert(Day20._01("20") == 608)
    }

    test("Day 20-02-00") {
        assert(Day20._02("20-02") == 396)
    }

    test("Day 20-02") {
        assert(Day20._02("20") == 6706)
    }

}
