import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import scala.annotation.tailrec
import scala.util.matching.Regex

object Day3 extends Exercise(2024, 3) {
  def run(input: List[String]): Unit = {

    part1(input)
    part2(input)
    ()
  }


  private def getMuls(input: List[String]): Long = {
    val pattern: Regex = """mul\(\d+,\d+\)""".r
    var result: Long = 0
    for (line <- input) {
      val matches = pattern.findAllIn(line).toList
      for (match_ <- matches){
        result += getMulResult(match_)
      }
    }
    result
  }

  private def getMulsDoDont(input: List[String]): Long = {
    val pattern: Regex = """mul\(\d+,\d+\)|do\(\)|don't\(\)""".r
    var result: Long = 0
    var do_flag: Boolean = true
    for (line <- input) {
      val matches = pattern.findAllIn(line).toList
        for (match_ <- matches){
            if (match_ == "do()") {
                do_flag = true
            }
            else if (match_ == "don't()") {
                do_flag = false
            }
            else if (do_flag) {
                result += getMulResult(match_)
            }
        }
    }

    result
  }

  private def getMulResult(input: String): Long = {
    val pattern: Regex = """mul\((\d+),(\d+)\)""".r
    val pattern(a, b) = input
    a.toInt * b.toInt
  }

  private def part1(input: List[String]): Long = part1 {
    getMuls(input)
  }

  private def part2(input: List[String]): Long = part2 {
    getMulsDoDont(input)
  }
}
