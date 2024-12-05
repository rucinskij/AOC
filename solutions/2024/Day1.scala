import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise

object Day1 extends Exercise(2024, 1) {
  def run(input: List[String]): Unit = {

    part1(input)
    part2(input)
    ()
  }

  case class Locations(el1: Int, el2: Int)

  object Locations{
    def apply(input: String): Locations = {
      val split = input.split("   ")
      Locations(split(0).toInt, split(1).toInt)
    }
  }


  private def prepInput(input: List[String]): List[Locations] = input.map(el => Locations(el))

  private def createLists(locations: List[Locations]): List[List[Int]] = 
    List(locations.map(_.el1).sorted, locations.map(_.el2).sorted)

  private def countDistance(sorted_lists: List[List[Int]]):Int  = {
      val list1 = sorted_lists(0)
      val list2 = sorted_lists(1)

      var distance = 0
      for (i <- 0 until list1.length) {
        distance += Math.abs(list1(i) - list2(i))
      }

      distance
    }

    private def create_counts(sorted_lists: List[List[Int]]): Map[Int, Int] = {
      val target_list = sorted_lists(1)

      var counts = Map[Int, Int]()
      var count = 0
      for (i <- 1 until target_list.length) {
        
        if (target_list(i) == target_list(i-1)) {
          count += 1
        }
        else {
          counts = counts + (target_list(i-1) -> count)
          count = 1
        }
      }
      counts
    }

    private def similarity_score(sorted_lists: List[List[Int]], counts: Map[Int, Int]): Int = {
      val input_list = sorted_lists(0)
      var score = 0
      for (i <- 0 until input_list.length) {
        val target = input_list(i)
        if (counts.contains(target)) {
          score += counts(target) * target
        }
      }
      score
    }
  
  private def part1(input: List[String]): Int = part1 {
    val inputData = prepInput(input)
    val lists = createLists(inputData)
    countDistance(lists)
  }

  private def part2(input: List[String]): Int = part2 {
    val inputData = prepInput(input)
    val lists = createLists(inputData)
    similarity_score(lists, create_counts(lists))
  }
}
