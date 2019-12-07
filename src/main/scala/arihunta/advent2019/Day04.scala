package arihunta.advent2019

object Day04 {
    
    final val input = 235741 to 706948
    
    def _01(): Int = {
    
        input.filter(it => {
            val str = it.toString()
            var found = false
            var ascending = true
            for (i <- 1 to str.length() - 1) {
                if (str(i) < str(i-1)) ascending = false
                if (str(i) == str(i-1)) found = true
            }
            found && ascending
        })
        .size
    
    }
    
    def _02(): Int = {
    
        input.filter(it => {
            val str = it.toString()
            var found = false
            var ascending = true
            for (i <- 1 to str.length() - 1) {
                if (str(i) < str(i-1)) ascending = false
                if (
                        str(i) == str(i-1)
                        && (i == 1 || str(i) != str(i-2))
                        && (i == 5 || str(i) != str(i+1))
                   ) found = true
            }
            found && ascending
        })
        .size
    }
    
}
