import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

object Day6 extends Exercise(2024, 6) {
  def run(input: List[String]): Unit = {

    part1(input)
    part2(input)
    ()
  }
  private def prepInput(input: List[String]): List[List[String]] = input.map(_.split("").toList)
  private def isValid(input: List[List[String]], x: Int, y: Int): Boolean = 
    x >= 0 && y >= 0 && x < input.length && y < input(0).length
  private def findStart(input: List[List[String]]): (Int, Int) = {
    for (i <- 0 until input.length) {
      for (j <- 0 until input(0).length) {
        if (input(i)(j) != "." && input(i)(j) != "#") return (i, j)
      }
    }
    (-1, -1)
  }
  private def getDirection(pointer: String): (Int, Int) = {
    pointer match {
      case "^" => (-1, 0)
      case "v" => (1, 0)
      case "<" => (0, -1)
      case ">" => (0, 1)
    }
  }

  private def rotate(pointer: (Int, Int)): (Int, Int) = {
    pointer match {
        case (-1, 0) => (0, 1)
        case (1, 0) => (0, -1)
        case (0, -1) => (-1, 0)
        case (0, 1) => (1, 0)
    }
  }

  private def move(input: List[List[String]], position: (Int, Int)): Int = {
    var x = position._1
    var y = position._2
    var direction = getDirection(input(x)(y))
    var count = 1
    var visited = Set[(Int, Int)]((x,y))
    while (isValid(input, x + direction._1, y + direction._2)) {

        if (input(x + direction._1)(y + direction._2) == "#" ){
            direction = rotate(direction)
        }
        if (isValid(input, x + direction._1, y + direction._2)) {
            x += direction._1
            y += direction._2
            if (!visited.contains((x, y))) {
                count += 1
                visited += ((x, y))
            }
            
        }

    }

    count
  }

  private def move2(input: List[List[String]], position: (Int, Int)): Set[(Int, Int)] = {
    var x = position._1
    var y = position._2
    var direction = getDirection(input(x)(y))
    var visited = Set[(Int, Int)]((x, y))
    while (isValid(input, x + direction._1, y + direction._2)) {
        if (input(x + direction._1)(y + direction._2) == "#") {
            direction = rotate(direction)
        }
        if (isValid(input, x + direction._1, y + direction._2)) {
            x += direction._1
            y += direction._2
            visited += ((x, y))
        }
    }
    visited
  }

  private def checkLoop(input: List[List[String]], position: (Int, Int)): Boolean = {
    var x = position._1
    var y = position._2
    var direction = getDirection(input(x)(y))
    var visited = Set[((Int, Int), (Int, Int))](((x, y), direction))
    while (isValid(input, x + direction._1, y + direction._2)) {
        if (input(x + direction._1)(y + direction._2) == "#") {
            direction = rotate(direction)
        }
        if (isValid(input, x + direction._1, y + direction._2)) {
            if (input(x + direction._1)(y + direction._2) != "#") {
                x += direction._1
                y += direction._2
                if (visited.contains(((x, y), direction))) {
                    return true
                }
                visited += (((x, y), direction))
            }
        }
    }
    false
  }
  
  

  private def part1(input: List[String]): Int = part1 {
    move(prepInput(input), findStart(prepInput(input)))
  }

  private def part2(input: List[String]): Int = part2 {
    val board = prepInput(input)
    val route = move2(board, findStart(board))
    var result = 0
    for (position <- route) {
        if (position != findStart(board)) {
            val newBoard = board.updated(position._1, board(position._1).updated(position._2, "#"))
            if (checkLoop(newBoard, findStart(board))) {
                result += 1 
            }
        }
    }
    result
  }
}
