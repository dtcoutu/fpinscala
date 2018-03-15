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

	"drop" should "drop the given number of elements" in {
		fpinscala.datastructures.List.drop(List(1,2,3), 1) shouldBe List(2,3)
		fpinscala.datastructures.List.drop(List(6,5,4,3,2,1), 3) shouldBe List(3,2,1)
		fpinscala.datastructures.List.drop(List(6,7,8,9), 4) shouldBe Nil
	}

	"dropWhile" should "drop elements until condition is false" in {
		fpinscala.datastructures.List.dropWhile(List(1,2,3), (a:Int) => a < 2) shouldBe List(2,3)
		fpinscala.datastructures.List.dropWhile(List(1,2,3), (a:Int) => a % 2 == 1) shouldBe List(2,3)
		fpinscala.datastructures.List.dropWhile(List(1,2,3), (a:Int) => a > 3) shouldBe List(1,2,3)
		fpinscala.datastructures.List.dropWhile(List(1,2,3), (a:Int) => a < 5) shouldBe Nil
		fpinscala.datastructures.List.dropWhile(Nil, (a:Int) => a < 5) shouldBe Nil
	}
}