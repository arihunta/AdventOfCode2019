package arihunta.advent2019

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day06Test extends FunSuite {
    
    test("Day 06-01") {
        assert(Day06._01() == 142497)
    }
    
    test("Day 06-02") {
        assert(Day06._02() == 301)
    }
    
}
