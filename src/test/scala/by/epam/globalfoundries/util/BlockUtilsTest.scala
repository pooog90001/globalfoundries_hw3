package by.epam.globalfoundries.util

import by.eapam.globalfoundries.util.{BlockUtils, PriorityBlock}
import org.scalatest.FunSuite

class BlockUtilsTest extends FunSuite {

  test("should compute stream and return total 23") {
    var list = List[PriorityBlock](
      PriorityBlock(1, 17, 21),
        PriorityBlock(1, 0, 3),
        PriorityBlock(1, 11, 25),
        PriorityBlock(1, 2, 7),
        PriorityBlock(1, 9, 14),
        PriorityBlock(1, 13, 18)
    ).toStream

    val expectedTotalBlockingTime = Map(1 -> 23)
    val  actualTotalBlockingTime = BlockUtils.getTotalBlockingTime(list)

    assert(actualTotalBlockingTime === expectedTotalBlockingTime)
  }

  test("should compute stream with several stages") {
    var list = List[PriorityBlock](
      PriorityBlock(2, 0, 3),
      PriorityBlock(2, 2, 7),
      PriorityBlock(2, 9, 14),

      PriorityBlock(3, 0, 3),
      PriorityBlock(3, 2, 7),
      PriorityBlock(3, 9, 14),

      PriorityBlock(1, 0, 3),
      PriorityBlock(1, 2, 7),
      PriorityBlock(1, 9, 14),

      PriorityBlock(3, 11, 25),
      PriorityBlock(3, 13, 18),
      PriorityBlock(3, 17, 21),

      PriorityBlock(2, 11, 25),
      PriorityBlock(2, 13, 18),
      PriorityBlock(2, 17, 21),

      PriorityBlock(1, 11, 22),
      PriorityBlock(1, 13, 18),
      PriorityBlock(1, 17, 21)
    ).toStream

    val expectedTotalBlockingTime = Map(1 -> 20, 2 -> 23, 3 -> 23)
    val  actualTotalBlockingTime = BlockUtils.getTotalBlockingTime(list)

    assert(actualTotalBlockingTime  === expectedTotalBlockingTime)
  }

  test("should compute unsorted stream with same startTime") {
    var list = List[PriorityBlock](
      PriorityBlock(1, 0, 2),
      PriorityBlock(4, 0, 3),
      PriorityBlock(5, 0, 2),
      PriorityBlock(6, 0, 1)
    ).toStream

    val expectedTotalBlockingTime = Map(1 -> 2, 4 -> 3, 5 -> 2, 6 -> 1)
    val  actualTotalBlockingTime = BlockUtils.getTotalBlockingTime(list)

    assert(actualTotalBlockingTime === expectedTotalBlockingTime)
  }

  test("should compute empty stream") {
    var list = List[PriorityBlock]().toStream

    val expectedTotalBlockingTime = Map()
    val  actualTotalBlockingTime = BlockUtils.getTotalBlockingTime(list)

    assert(actualTotalBlockingTime === expectedTotalBlockingTime)
  }

  test("should throw IllegalArgumentException") {

    val exception = intercept[IllegalArgumentException] {
      BlockUtils.generateStream(0, 3)
    }

    assert(exception !== null)
  }

  test("should generate correct stream with timeLimit = Integer.MAX_VALUE") {
    val priorityLimit = 5
    val timeLimit = Integer.MAX_VALUE

    val stream = BlockUtils.generateStream(priorityLimit, timeLimit).take(1000)

    stream.foreach(block => {
      assert(block.priority <= priorityLimit)
      assert(block.priority >= 0)

      assert(block.endTime <= Integer.MAX_VALUE)
      assert(block.startTime <= block.endTime)
      assert(block.startTime >= 0)
    })
  }

  test("should generate correct stream with timeLimit = 1") {
    val priorityLimit = 5
    val timeLimit = 1
    val stream = BlockUtils.generateStream(priorityLimit, timeLimit).take(100000)

    stream.foreach(block => {
      assert(block.priority <= priorityLimit)
      assert(block.priority >= 0)

      assert(block.endTime <= Integer.MAX_VALUE)
      assert(block.startTime <= block.endTime)
      assert(block.startTime >= 0)
    })
  }

}
