import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day4 extends Exercise(2024, 4) {
  def run(input: List[String]): Unit = {

    val test_input = List(
        "MMMSXXMASM",
        "MSAMXMSMSA",
        "AMXSXMAAMM",
        "MSAMASMSMX",
        "XMASAMXAMM",
        "XXAMMXXAMA",
        "SMSMSASXSS",
        "SAXAMASAAA",
        "MAMMMXMMMM",
        "MXMXAXMASX"
    )

    part1(input)
    part2(input)
    ()
  }

  private def prepInput(input: List[String]): List[List[String]] = input.map(_.split("").toList)

  private def isValid(input: List[List[String]], x: Int, y: Int): Boolean = 
        x >= 0 && y >= 0 && x < input.length && y < input(0).length

  private def findXMAS(input: List[List[String]], position: (Int, Int)): Int = {
    val directions = List(
        (0, 1),
        (1, 0),
        (0, -1),
        (-1, 0),
        (1, 1),
        (1, -1),
        (-1, 1),
        (-1, -1)
    )



    def findWord(x: Int, y: Int, dx: Int, dy: Int, word: String): Boolean = {
        var i = 0
        while (i < word.length) {
            val nx = x + (i+1) * dx
            val ny = y + (i+1) * dy
            if (!isValid(input, nx, ny) || input(nx)(ny) != word(i).toString) return false
            i += 1
        }
        true
    }

    val (startX, startY) = position
    var count = 0
    for ((dx, dy) <- directions) {
        if (findWord(startX, startY, dx, dy, "MAS")) {
            count += 1
        }
    }
    count
  }

  private def findX_MAS(input: List[List[String]], position: (Int, Int)): Int = {

    def findWord(x: Int, y: Int, dx: Int, dy: Int, word: String): Boolean = {
        var i = 0
        while (i < word.length) {
            val nx = x + (i+1) * dx
            val ny = y + (i+1) * dy
            if (!isValid(input, nx, ny) || input(nx)(ny) != word(i).toString) return false
            i += 1
        }
        true
    }

    val (startX, startY) = position
    var count = 0
    if ((findWord(startX, startY, 1, 1, "M") && findWord(startX, startY, 1, -1, "M") && findWord(startX, startY, -1, -1, "S") && findWord(startX, startY, -1, 1, "S")) || //M z prawej S z lewej
      (findWord(startX, startY, -1, -1, "M") && findWord(startX, startY, -1, 1, "M") && findWord(startX, startY, 1, 1, "S") && findWord(startX, startY, 1, -1, "S")) || //M z lewej S z prawej
      (findWord(startX, startY, 1, -1, "M") && findWord(startX, startY, -1, -1, "M") && findWord(startX, startY, -1, 1, "S") && findWord(startX, startY, 1, 1, "S")) || //M z gory S z dolu
      (findWord(startX, startY, -1, 1, "M") && findWord(startX, startY, 1, 1, "M") && findWord(startX, startY, 1, -1, "S") && findWord(startX, startY, -1, -1, "S"))) { //M z dolu S z gory
      count += 1
    }

    count
  }
  private def allXMAS(input: List[List[String]]): Int = {
    var result = 0
    for (i <- 0 until input.length) {
        for (j <- 0 until input(0).length) {

            if (input(i)(j) == "X") {
                result += findXMAS(input, (i, j))
            }
        }
    }
    result
  }

  private def allX_MAS(input: List[List[String]]): Int = {
    var result = 0
    for (i <- 0 until input.length) {
        for (j <- 0 until input(0).length) {
            if (input(i)(j) == "A") {
                result += findX_MAS(input, (i, j))
            }
        }
    }
    result
  }

  private def part1(input: List[String]): Int = part1 {
    allXMAS(prepInput(input))
  }

  private def part2(input: List[String]): Int = part2 {
    allX_MAS(prepInput(input))
  }
}
