import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import scala.math.pow

object Day7 extends Exercise(2024, 7) {
  def run(input: List[String]): Unit = {

    part1(input)
    part2(input)
    ()
  }
  
  private def prepInput(input: List[String]): Map[Long, List[Int]] = {
    input.map { line =>
        val parts = line.split(":").map(_.trim)
        val key = parts(0).toLong
        val values = parts(1).split(" ").map(_.trim.toInt).toList
        key -> values
    }.toMap
  }

  private def checkOperators(result: Long, factors: List[Int]): Boolean = {


    def helper(current: Long, remaining: List[Int]): Boolean = {
      if (current > result) false
      else if (remaining.isEmpty) current == result
      else {
        val next = remaining.head
        helper(current + next, remaining.tail) || helper(current * next, remaining.tail)
      }
    }

    helper(factors.head, factors.tail)

  }

  private def concatenate(val1: Long, val2: Int): Long = {
    val tens = val2.toString.length
    val1 * pow(10,tens).toLong + val2
  }

  private def checkOperators2(result: Long, factors: List[Int]): Boolean = {


    def helper(current: Long, remaining: List[Int]): Boolean = {
      if (current > result) false
      else if (remaining.isEmpty) current == result
      else {
        val next = remaining.head
        helper(current + next, remaining.tail) || helper(current * next, remaining.tail) || helper(concatenate(current, next), remaining.tail)
      }
    }

    helper(factors.head, factors.tail)

  }



  private def part1(input: List[String]): Long = part1 {
    val equations = prepInput(input)
    var result: Long = 0
    for ((key, value) <- equations) {
      if (checkOperators(key, value))
        result += key
    }
    result
  }
  

  private def part2(input: List[String]): Long = part2 {
    val equations = prepInput(input)
    var result: Long = 0
    for ((key, value) <- equations) {
      if (checkOperators2(key, value))
        result += key
    }
    result
  }
}
