package arihunta.advent2019

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class Day05Test extends FunSuite {
    
    test("Day 05-01") {
        assert(Day05._01() == 5821753)
    }
    
    test("Day 05-02") {
        assert(Day05._02() == 11956381)
    }

}

