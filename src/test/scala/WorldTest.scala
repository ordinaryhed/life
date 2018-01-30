

import org.scalatest.{FunSuite, Matchers}


class WorldTest extends FunSuite with Matchers {

  test("increasing the accumulated value of an account") {
    World.apply(1, 2, Set(new Wood(), new FreshWater()), Set(new AnimalGroup("Human", 3)))

    World.findHex(1, 2) shouldNot be(null)
    World.findHex(1, 2) match {
      case Some(h) => h.resources.size should be(2)
      case _ => fail()
    }

    World.findHex(1, 0) match {
      case Some(h) => fail()
      case _ => succeed
    }

    World.findHex(1, 2) match {
      case Some(h) => h.availableFoodFromNature() should be(new Wood().food() + new FreshWater().food())
      case _ => fail()
    }

    println(World.toString)
  }

  test("tick on") {
    World.apply(1, 2, Set(new Wood(1), new FreshWater(6)))
    World.findHex(1, 2) match {
      case Some(h) => h.resources.size should be(2)
    }

    World.findHex(1, 2) match {
      case Some(h) => {
        h.resources.collect({
          case f: FreshWater => {
            f.level should be(6)
          }
          case w: Wood => {
            w.level should be(1)
          }
        })

        val rs = h.tickResourcesInHex()

        rs.collect({
          case f: FreshWater => {
            f.level should be(6)
          }
          case w: Wood => {
            w.level should be(2)
          }
        })

        World.apply(1, 2, rs)
      }
    }

    World.findHex(1, 2) match {
      case Some(h) => {
        h.resources.collect({
          case w: Wood => {w.level should be(2)}
        })
      }
    }

    World.tick()
    World.findHex(1, 2) match {
      case Some(h) => {
        h.resources.collect({
          case w: Wood => {w.level should be(3)}
        })
      }
    }
  }

  test("one hed world") {
    World.apply(1, 2, Set(new Wood(1), new FreshWater(6)), Set(new AnimalGroup("Human", 1)))
    World.findHex(1,2).get.animals.head.gSize should be(1)
    World.tick()
    World.findHex(1,2).get.resources.collect({case (a:Wood) => a.level should be(2)})
    World.findHex(1,2).get.animals.head.gSize should be(1)
    World.tick()
    World.findHex(1,2).get.resources.collect({case (a:Wood) => a.level should be(3)})
    World.findHex(1,2).get.animals.head.gSize should be(2)

  }

  test("print") {
    World.apply(1, 2, Set(new Wood(1), new FreshWater(6)), Set(new AnimalGroup("Human", 1)))
    World.findHex(1,2).get.mapString(1, 2, 0) should be ("W1F6")
    println("------")
    println(World.findHex(1,2).get.mapString(1, 2, 0))
    println("------")
  }

  test("pretty world in print") {
    World.apply(1, 2, Set(new Wood(1), new FreshWater(6)), Set(new AnimalGroup("Human", 1)))
    World.apply(2, 2, Set(new Wood(1), new FreshWater(4)), Set(new AnimalGroup("Human", 3)))
    World.apply(2, 3, Set(new FreshWater(3)))
    World.apply(2, 4, Set(new FreshWater(1)), Set(new AnimalGroup("Human", 3)))
    World.apply(1, 3, Set(new Wood(1)))

    println(World.toString)
    World.tick()
    println(World.toString)
    World.tick()
    println(World.toString)

  }

}