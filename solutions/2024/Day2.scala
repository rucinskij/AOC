import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day2 extends Exercise(2024, 2) {
  def run(input: List[String]): Unit = {

    part1(input)
    part2(input)
    ()
  }

  object Reports{
    def apply(input: String): List[Int] = {
      val split = input.split(" ")
      split.map(_.toInt).toList
    }
  }

  private def prepInput(input: List[String]): List[List[Int]] = input.map(el => Reports(el))

  private def checkLevel(level: List[Int]): Boolean = {
    val increasing = level(0) < level(1)
    var isgood = true
    for (i <- 0 until level.length-1) {
        if (increasing && level(i) >= level(i+1)) {
            isgood = false
        }
        else if (!increasing && level(i) <= level(i+1)) {
            isgood = false
        }
        else if (Math.abs(level(i) - level(i+1)) > 3) {
            isgood = false
        }
    }
    return isgood
  }

  private def checkLevel2(level: List[Int]): Boolean = {
    var isgood = checkLevel(level)
    if (!isgood) {
        for (i <- 0 until level.length) {
            val removed_list = level.slice(0, i) ++ level.slice(i+1, level.length)
            if (checkLevel(removed_list)) {
                isgood = true
            }
        }
    }
    return isgood

  }

  private def part1(input: List[String]): Int = part1 {
    val reports = prepInput(input)
    val valid_reports = reports.filter(checkLevel)
    valid_reports.length
  }

  private def part2(input: List[String]): Int = part2 {
    val reports = prepInput(input)
    val valid_reports = reports.filter(checkLevel2)
    valid_reports.length
  }
}
