import com.pg.bigdata.neighborhoodanalytics.aoc.imperative.Exercise
object Day5 extends Exercise(2024, 5) {
  def run(input: List[String]): Unit = {

    part1(input)
    part2(input)
    ()
  }

  private def prepInput(input: List[String]): (List[String],List[List[Int]]) = {
    val (rules, updates) = input.span(_ != "")
    val updatesList = updates.drop(1).map(_.split(",").toList).map(_.map(_.toInt))
    (rules, updatesList)

  }

  private def createRuleMap(rules: List[String]): Map[Int, List[Int]] = {
    var ruleMap = Map[Int, List[Int]]()
    for (rule <- rules) {
        val split = rule.split("\\|")
        val key = split(0).toInt
        val value = split(1).toInt
        if (ruleMap.contains(key)) {
            if (!ruleMap(key).contains(value)) {
                ruleMap = ruleMap.updated(key, ruleMap(key) :+ value)
            } 
        }
        else {
            ruleMap += (key -> List(value))
        }
        
 
    }
    ruleMap
  }

  private def getMiddle(update: List[Int]): Int = {
    val mid = update.length / 2
    update(mid)
  }

  private def checkUpdate(ruleMap: Map[Int, List[Int]], update: List[Int]): Int = {
    val reversedUpdate = update.reverse
    for (i <- 0 until reversedUpdate.length) {
        if (ruleMap.contains(reversedUpdate(i))) {
            val update_rules = ruleMap(reversedUpdate(i))
            for (j <- i + 1 until reversedUpdate.length) {
                if (update_rules.contains(reversedUpdate(j))) {
                    return 0
                }
            }
        }

    }
    return getMiddle(reversedUpdate)
  }

  private def fixUpate(ruleMap: Map[Int, List[Int]], update: List[Int]): List[Int] = {
    var new_update = List[Int]()
    for (value <- update) {
        if (ruleMap.contains(value)) {
            val ruleValues = ruleMap(value)
            var inserted = false
            for (ruleValue <- ruleValues) {
                val index = new_update.indexOf(ruleValue)
                if (index != -1) {
                    new_update = new_update.patch(index, Seq(value), 0) // jezeli element z reguly jest w liscie to wrzucamy przed niego nasz nowy element
                    inserted = true
                }
            }
            if (!inserted) {
                new_update :+= value
            }
        } else {
            new_update :+= value
        }
    }
    return new_update
  }

  private def keepFirstOccurence(update: List[Int]): List[Int] = {
    var new_update = List[Int]()
    for (value <- update) {
        if (!new_update.contains(value)) {
            new_update :+= value
        }
    }
    new_update
  }

  private def part1(input: List[String]): Int = part1 {
    val data = prepInput(input)
    val rules = data._1
    val updates = data._2
    val ruleMap = createRuleMap(rules)
    var result = 0
    for (update <- updates) {
        result += checkUpdate(ruleMap, update)
    }

    result
  }

  private def part2(input: List[String]): Int = part2 {
    val data = prepInput(input)
    val rules = data._1
    val updates = data._2
    val ruleMap = createRuleMap(rules)
    var result = 0
    for (update <- updates) {
        if (checkUpdate(ruleMap, update) == 0) {
            val fixedUpdate = fixUpate(ruleMap, update)
            result += getMiddle(keepFirstOccurence(fixedUpdate))
            }
    
    }
    result
    }
}