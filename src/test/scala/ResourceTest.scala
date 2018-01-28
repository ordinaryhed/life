import org.scalatest.{FunSuite, Matchers}

class ResourceTest  extends FunSuite with Matchers {
  test("increasing the accumulated value of an account") {
    new Wood(3).tickResource(Set(new FreshWater(3), new Wood(3)))
      .collect{
        case a: Wood => a.level should be (3)
        case b: FreshWater => b.level should be (3)
      }

    var w = new Wood(3)
    val r = w.tickResource(Set(new FreshWater(4), w))
      r
      .collect{
        case a: Wood => a.level should be (4)
        case b: FreshWater => b.level should be (4)
      }

  }
}
