package arihunta.advent2019


object IntCodeComputer {

    def run(program: Array[Int], input: Int): Int = {

        var position = 0
        
        var output = -1

        while (program(position) != 99) {

            val instruction = "%05d".format(program(position))
            
            val opcode = instruction.substring(3, 5).toInt
            val param3mode = Integer.parseInt(instruction(0).toString())
            val param2mode = Integer.parseInt(instruction(1).toString())
            val param1mode = Integer.parseInt(instruction(2).toString())
    
            opcode match {
                case 1 => {
                    val param1 = param(program, position, param1mode, 1)
                    val param2 = param(program, position, param2mode, 2)
                    program(program(position + 3)) = param1 + param2
            		position += 4
                }
                case 2 => {
                    val param1 = param(program, position, param1mode, 1)
                    val param2 = param(program, position, param2mode, 2)
                    program(program(position + 3)) = param1 * param2
            		position += 4
                }
                case 3 => {
                    program(program(position + 1)) = input
                    position += 2
                }
                case 4 => {
                    output = param(program, position, param1mode, 1)
                    position += 2
                }
                case 5 => {
                    position =
                        if (param(program, position, param1mode, 1) != 0)
                            param(program, position, param2mode, 2)
                        else position + 3
                }
                case 6 => {
                    position =
                        if (param(program, position, param1mode, 1) == 0)
                            param(program, position, param2mode, 2)
                        else position + 3
                }
                case 7 => {
                    program(program(position + 3)) = 
                        if (param(program, position, param1mode, 1) < param(program, position, param2mode, 2)) 1
                        else 0
            		position += 4
                }
                case 8 => {
                    program(program(position + 3)) = 
                        if (param(program, position, param1mode, 1) == param(program, position, param2mode, 2)) 1
                        else 0
            		position += 4
                }
            }
        }

        output

    }
    
    private def param(program: Array[Int], position: Int, mode: Int, param: Int): Int = {
        if (mode == 1) program(position + param) else program(program(position + param))
    }

}
