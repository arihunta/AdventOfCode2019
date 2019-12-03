package arihunta.advent2019

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day01Test extends FunSuite {

    test("Day 01-01") {
        assert(Day01._01() == 3361299)
    }

    test("Day 01-02") {
        assert(Day01._02() == 5039071)
    }

}
