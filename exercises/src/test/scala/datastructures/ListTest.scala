package fpinscala.datastructures

import org.scalatest.{FlatSpec, Matchers}

class ListTest extends FlatSpec with Matchers {
	"tail" should "drop the first element" in {
		fpinscala.datastructures.List.tail(List(1,2,3)) shouldBe List(2,3)
		fpinscala.datastructures.List.tail(Nil) shouldBe Nil
		fpinscala.datastructures.List.tail(List("a", "z", "y")) shouldBe List("z", "y")
	}

	"setHead" should "add element to beginning of given list" in {
		fpinscala.datastructures.List.setHead(List(2,3), 1) shouldBe List(1,2,3)
		fpinscala.datastructures.List.setHead(Nil, 1) shouldBe List(1)
	}
}