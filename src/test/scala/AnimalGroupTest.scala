import org.scalatest.{FunSuite, Matchers}

class AnimalGroupTest extends FunSuite with Matchers {
  test("tick") {
    val group = new AnimalGroup("Human", 2)
    group.foodNeeded() should be (2)
    group.tick(4).gSize should be (3)
    group.tick(3).gSize should be (3)
    group.tick(2).gSize should be (2)
    group.tick(1).gSize should be (1)
    group.tick(0).gSize should be (1)
    group.tick(4).rece should be ("Human")
    group.tick(4).foodNeeded() should be (3)
  }

}
