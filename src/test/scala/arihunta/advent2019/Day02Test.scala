package arihunta.advent2019

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day02Test extends FunSuite {

    test("Day 02-01") {
        assert(Day02._01() == 2842648)
    }

    test("Day 02-02") {
        assert(Day02._02() == 9074)
    }

}
