package list
import org.scalatest.funsuite.AnyFunSuite
import list.implementation.SinglyLinkedIntList


class IntListTest extends AnyFunSuite {


  test("test object apply") {
    assert(SinglyLinkedIntList(4, 5, 6).head === 4)
  }

  test("test object apply tail") {
    assert(SinglyLinkedIntList(4, 5, 6).tail === SinglyLinkedIntList(5, 6))
  }

  test("testIsEmpty positive") {
    assert(SinglyLinkedIntList().isEmpty === true)
  }

  test("testIsEmpty negative") {
    assert(SinglyLinkedIntList(1, 2).isEmpty === false)
  }

  test("testContains positive") {
    assert(SinglyLinkedIntList(4, 5, 6).contains(4) === true)
  }

  test("testSize empty") {
    assert(SinglyLinkedIntList().size === 0)
  }

  test("testSize nonempty") {
    assert(SinglyLinkedIntList(4, 5, 6).size === 3)
  }

  test("testContains negative") {
    assert(SinglyLinkedIntList(4, 5, 6).contains(42) === false)
  }

  test("testGet") {
    assert(SinglyLinkedIntList(4, 5, 6).get(2) === 6)
  }

  test("testPrepend") {
    assert(SinglyLinkedIntList(4, 5, 6).prepend(3) === SinglyLinkedIntList(3, 4, 5, 6))
  }

  test("testAppend") {
    assert(SinglyLinkedIntList(4, 5, 6).append(7) === SinglyLinkedIntList(4, 5, 6, 7))
  }

  test("testPrefix") {
    assert(SinglyLinkedIntList(4, 5, 6).prefix(SinglyLinkedIntList(1, 2, 3)) === SinglyLinkedIntList(1, 2, 3, 4, 5, 6))
  }

  test("testDelete") {
    assert(SinglyLinkedIntList(4, 5, 6, 6, 7, 6).delete(6) === SinglyLinkedIntList(4, 5, 6, 7, 6))
  }

  test("testDeleteAll") {
    assert(SinglyLinkedIntList(4, 5, 6, 6, 7).deleteAll(6) === SinglyLinkedIntList(4, 5, 7))
  }

  test("testFlipList") {
    assert(SinglyLinkedIntList(4, 5, 6, 6, 7).flip === SinglyLinkedIntList(7, 6, 6, 5, 4))
  }

  test("testMap") {
    assert(SinglyLinkedIntList(1, 2, 3).map(x => x * x) === SinglyLinkedIntList(1, 4, 9))
  }

  test("testFilter") {
    assert(SinglyLinkedIntList(1, 2, 3).filter(x => x > 2) === SinglyLinkedIntList(3))
  }

  test("testFoldLeft") {
    assert(SinglyLinkedIntList(1, 2, 3).foldLeft(5)((x, y) => x + y) === 11)
  }

  test("testFoldLeft is left to right") {
    assert(SinglyLinkedIntList(1, 2, 3).foldLeft(0)((x, y) => {
      if (x > y)
        fail()
      else y
    }) === 3)
  }
  test("testFoldLeft is left to right 2") {
    assert(SinglyLinkedIntList(1, 2, 3, 4, 5).foldLeft(10)((x, y) => x-y) === -5)
  }

  test("testReduceLeft is left to right") {
    assert(SinglyLinkedIntList(1, 2, 3).reduceLeft((x, y) => {
      if (x > y)
        fail()
      else y
    }) === 3)
  }

  test("testReduceLeft is left to right 2") {
    assert(SinglyLinkedIntList(1, 2, 3, 4 , 5).reduceLeft((x, y) => x-y) === -13)
  }

  test("testForAll positive") {
    assert(SinglyLinkedIntList(1, 2, 3).forAll(x => x > 0) === true)
  }

  test("testForAll negative") {
    assert(SinglyLinkedIntList(1, 2, 3).forAll(x => x > 3) === false)
  }

  test("testForAll returns on first negative") {
    var c = 0
    SinglyLinkedIntList(1, 2, 3, 4, 5, 6).forAll(x => {
      c += 1
      x < 3
    })
    assert(c == 3)
  }

  test("testFoldRight") {
    assert(SinglyLinkedIntList(1, 2, 3).foldRight(5)((x, y) => x + y) === 11)
  }

  test("testFoldRight is right to left") {
    assert(SinglyLinkedIntList(3, 2, 1).foldRight(0)((x, y) => {
      if (x > y)
        x
      else fail()
    }) === 3)
  }
  test("testFoldRight is right to left 2") {
    assert(SinglyLinkedIntList(1, 2, 3,4,5).foldRight(10)((x, y) => x-y) === -7)
  }

  test("testReduceRight") {
    assert(SinglyLinkedIntList(1, 2, 3).reduceRight(_ + _) === 6)
  }

  test("testReduceRight is right to left") {
    assert(SinglyLinkedIntList(1, 2, 3, 4 , 5).reduceRight((x, y) => x-y) === 3)
  }
  test("testInsertionSort") {
    assert(SinglyLinkedIntList(5, 1, 2, 4, 3).insertionSort == SinglyLinkedIntList(1, 2, 3, 4, 5))
  }
  test("testInsertSorted") {
    assert(SinglyLinkedIntList(1, 2, 3, 4).insertSorted(5) === SinglyLinkedIntList(1, 2, 3, 4, 5))
  }

   test("testInsertSorted2") {
    assert(SinglyLinkedIntList(1, 2, 3, 8).insertSorted(5) === SinglyLinkedIntList(1, 2, 3, 5, 8))
  }
    test("testInsertSorted3") {
    assert(SinglyLinkedIntList(1, 2, 3, 4,6).insertSorted(5) === SinglyLinkedIntList(1, 2, 3, 4, 5,6))
  }
}