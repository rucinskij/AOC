import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
import scala.math.pow

object Day8 extends Exercise(2024, 8) {
  def run(input: List[String]): Unit = {
    part1(input)
    part2(input)

  }

  private def prepInput(input: List[String]): List[List[String]] = input.map(_.split("").toList)

  private def isValid(input: List[List[String]], x: Int, y: Int): Boolean = 
    x >= 0 && y >= 0 && x < input.length && y < input(0).length

  private def createAntenaMap(input: List[List[String]]): Map[String, List[(Int, Int)]] = {
    var antenaMap = Map[String, List[(Int, Int)]]()
    for (i <- input.indices) {
        for (j <- input(i).indices) {
        if (input(i)(j) != ".") {
            if (antenaMap.contains(input(i)(j))) {
             antenaMap = antenaMap.updated(input(i)(j), antenaMap(input(i)(j)) :+ (i, j))
            } else {
                antenaMap += (input(i)(j) -> List((i, j)))
            }
        }
        }
    }
    antenaMap
  }

  private def getAntinodes(antenas: List[(Int, Int)]): List[((Int, Int),(Int, Int))] = {
    val antinodes = for {
        i <- antenas.indices
        j <- i + 1 until antenas.length
    } yield {
        
        val (x1, y1) = antenas(i)
        val (x2, y2) = antenas(j)
        val dx = x2 - x1
        val dy = y2 - y1
        val antinode1 = ((x1 - dx), (y1 - dy))
        val antinode2 = ((x2 + dx), (y2 + dy))
        (antinode1, antinode2)
                    
    }
    antinodes.toList
  }
private def getAntinodes2(input: List[List[String]], antenas: List[(Int, Int)]): List[(Int, Int)] = {
    val antinodes = for {
        i <- antenas.indices
        j <- i + 1 until antenas.length
    } yield {
        val (x1, y1) = antenas(i)
        val (x2, y2) = antenas(j)
        val dx = x2 - x1
        val dy = y2 - y1
        var mul = 0
        var nodes = List[(Int, Int)]()
        while (isValid(input, x1 - dx * mul, y1 - dy * mul) || isValid(input, x2 + dx * mul, y2 + dy * mul)) {
            if (isValid(input, x1 - dx * mul, y1 - dy * mul)) {
                nodes = (x1 - dx * mul, y1 - dy * mul) :: nodes
            }
            if (isValid(input, x2 + dx * mul, y2 + dy * mul)) {
                nodes = (x2 + dx * mul, y2 + dy * mul) :: nodes
            }
            mul += 1
        }
        nodes
    }
    antinodes.flatten.toList
}




  private def part1(input: List[String]): Int = part1 {
    var result = 0
    var total_antinodes = Set[(Int, Int)]()
    for ((k, v) <- createAntenaMap(prepInput(input))) {
        val antinodes = getAntinodes(v)
        for (antinode <- antinodes) {
            total_antinodes += antinode._1
            total_antinodes += antinode._2
        }
        
    }
    for (antinode <- total_antinodes) {
        if (isValid(prepInput(input), antinode._1, antinode._2)) {
            result += 1
        }
    }

    result
  }
  

  private def part2(input: List[String]): Int = part2 {

    var total_antinodes = Set[(Int, Int)]()
    for ((k, v) <- createAntenaMap(prepInput(input))) {
        val antinodes = getAntinodes2(prepInput(input), v)
        for (antinode <- antinodes) {
            total_antinodes += antinode
        }
        
    }
    total_antinodes.size

 }
}