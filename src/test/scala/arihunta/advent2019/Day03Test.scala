package arihunta.advent2019

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day03Test extends FunSuite {

    test("Day 03-01") {
        assert(Day03._01() == 217)
    }

    test("Day 03-02") {
        assert(Day03._02() == 3454)
    }

}
