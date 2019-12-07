package arihunta.advent2019

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day04Test extends FunSuite {

    test("Day 04-01") {
        assert(Day04._01() == 1178)
    }

    test("Day 04-02") {
        assert(Day04._02() == 763)
    }

}
