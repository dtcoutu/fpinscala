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
}