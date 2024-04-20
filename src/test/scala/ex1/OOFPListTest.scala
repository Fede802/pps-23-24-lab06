package ex1

import org.scalatest.matchers.should.Matchers.*

class OOFPListTest extends org.scalatest.funsuite.AnyFunSuite:

  val reference: List[Int] = List(1, 2, 3, 4)

  test("test list methods implementation"):
    reference.zipWithValue(10) shouldBe List((1,10),(2, 10),(3, 10),(4, 10))
    reference.length() shouldBe 4
    reference.zipWithIndex shouldBe List((1,0),(2,1),(3,2),(4,3))
    reference.partition(_ % 2 == 0) shouldBe (List(2, 4), List(1, 3))
    reference.span(_ % 2 != 0) shouldBe (List(1), List(2, 3, 4))
    reference.span(_ < 3) shouldBe (List(1, 2), List(3, 4))
    reference.reduce(_ + _) shouldBe 10
    List(10).reduce(_ + _) shouldBe 10
    reference.takeRight(3) shouldBe List(2, 3, 4)
    reference.collect { case x if x % 2 == 0 => x + 1 } shouldBe List(3, 5)


