package arihunta.advent2019

import scala.collection.mutable.PriorityQueue

import org.scalatest.FunSuite

class Day18Test extends FunSuite {

    test("Day 18-01-00") {
        assert(Day18._01("testdata/18-01-00") == 8)
    }

    test("Day 18-01-01") {
        assert(Day18._01("testdata/18-01-01") == 86)
    }

    test("Day 18-01-02") {
        assert(Day18._01("testdata/18-01-02") == 132)
    }

    test("Day 18-01-03") {
        assert(Day18._01("testdata/18-01-03") == 136)
    }

    test("Day 18-01-04") {
        assert(Day18._01("testdata/18-01-04") == 81)
    }

    test("Day 18-01") {
        assert(Day18._01("18-01") == 5198)
    }

    test("18-02-00") {
        assert(Day18._02("testdata/18-02-00") == 8)
    }

    test("18-02-01") {
        assert(Day18._02("testdata/18-02-01") == 24)
    }

    test("18-02-02") {
        assert(Day18._02("testdata/18-02-02") == 32)
    }

    test("18-02-03") {
        assert(Day18._02("testdata/18-02-03") == 72)
    }

    test("18-02") {
        assert(Day18._02("18-02") == 1736)
    }

}
