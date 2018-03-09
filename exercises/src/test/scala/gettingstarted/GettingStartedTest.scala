package fpinscala.gettingstarted

import org.scalatest.{FlatSpec, Matchers}

class GettingStartedTest extends FlatSpec with Matchers {
	"fib" should "return 0 given 1" in {
		MyModule.fib(1) shouldBe 0
	}

	"fib" should "return 1 given 2" in {
		MyModule.fib(2) shouldBe 1
	}

	"fib" should "return 1 given 3" in {
		MyModule.fib(3) shouldBe 1
	}

	"fib" should "return 5 given 6" in {
		MyModule.fib(6) shouldBe 5
	}

	"isSorted" should "work for a standard numeric sort" in {
		PolymorphicFunctions.isSorted(Array(1,2,8,10), (a: Int, b: Int) => a < b) shouldBe true
		PolymorphicFunctions.isSorted(Array(1,2,19,10), (a: Int, b: Int) => a < b) shouldBe false
	}
}