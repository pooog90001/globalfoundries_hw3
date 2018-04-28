package by.eapam.globalfoundries.util

import scala.util.Random

object BlockUtils {


  /*
  * generate infinity stream of PriorityBlocks
  */
  def generateStream(priorityLimit: Int, timeLimit: Int): Stream[PriorityBlock] = {
    if (priorityLimit <= 0 || timeLimit <= 0) {
      throw new IllegalArgumentException("Input values must be more then zero")
    }

    val random = Random

    Stream.continually({
      val priority = random.nextInt(priorityLimit)
      val endTime = random.nextInt(timeLimit)
      var startTime = 0

      if (endTime > 0) {
        startTime = endTime - random.nextInt(endTime)
      }
      PriorityBlock(priority, startTime, endTime)
    })

  }

  /*
  * compute total blocking time by priority by task
  * return map: key - proirity; value - total time this priority
  */
  def getTotalBlockingTime(listBlocks: Stream[PriorityBlock]): Map[Int, Int] = {

    // map of total time and max end time per every priority
    var valuesByPriorities = Map[Int, (Int, Int)]()

    sortByTimeStart(listBlocks).foreach(block => {
      var totalTime = 0
      var maxEndTime = 0

      val isPriorityExist = valuesByPriorities.contains(block.priority)

      if (isPriorityExist) {
        var values = valuesByPriorities(block.priority)
        totalTime = values._1
        maxEndTime = values._2
      }

      if (maxEndTime <= block.startTime) {
        totalTime += block.endTime - block.startTime

      } else if (maxEndTime < block.endTime) {
        totalTime += block.endTime - maxEndTime
      }
      if (maxEndTime < block.endTime) {
        maxEndTime = block.endTime
      }
      valuesByPriorities += (block.priority -> (totalTime, maxEndTime))

    })

    valuesByPriorities.map { case (priority, (totalTime, _)) => (priority, totalTime) }
  }

  /*
  * Just sort list of blocks by timeStart field
  */
  def sortByTimeStart(listBlocks: Stream[PriorityBlock]): Stream[PriorityBlock] = {
    listBlocks.sortBy(block => block.startTime)
  }


}
