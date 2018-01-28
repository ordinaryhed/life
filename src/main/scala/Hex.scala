import World.world

import scala.util.Random

case class Hex(resource: Set[Resource]) {
  def resources(): Set[Resource] = {resource}

  def collectFood(): Int = {
    resource
      .map(r => r.food())
      .sum
  }

  def toMapString():String = {
    resource
      .map(r => "" + r.getClass.getSimpleName.charAt(0) + r.lev())
      .mkString(" ")
  }


  def tickResourcesInHex(): Set[Resource] = {
    val rr = resource
      .collect({ case (r: Resource) =>  r.tickResource(resource)}).flatten

    println ("---" + rr)
    rr
  }

}


object World {
  var world : Map[(Int, Int), Hex] = Map.empty

  def apply(x:Int, y:Int, resource: Set[Resource]) : Hex = {
    val land = new Hex(resource)
    world += ((x,y) -> land)
    land
  }

  def findHex(x:Int, y:Int):Option[Hex] = {
    val h = world.get(x,y)
    println(h)
    h
  }


  def resources(): Set[Resource] = {
    val ran:Int = Random.nextInt(3)
    if (ran.equals(0)) {
      Set(new FreshWater(3))
    }
    else if (ran == 1) {
      Set(new Wood(1), new FreshWater(3))
    }
    else  {
      Set(new Wood(1))
    }
  }

  def tick(): Unit = {
    world.collect({
      case ((x: Int, y:Int), h: Hex) =>  {
        World.apply(x, y, h.resources()
          .collect({case (r: Resource) =>  r.tickResource(h.resources())})
          .flatten)
      }
    })
  }


  override def toString: String = {

    val minX:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (x) }).min
    val maxX:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (x) }).max
    val minY:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (y) }).min
    val maxY:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (y) }).max

    var out = ""

    out += "          "
    for( x <- minX until maxX+1 ) {
        out += f"${"(" + x + ")"}%10s"
    }
    out += "\n"
    out += "\n"


    for( y <- minY until maxY+1 ) {
      out += f"${"(" + y + ")"}%10s"

      for( x <- minX until maxX+1 ) {
        findHex(x, y) match {
          case Some(h) =>
            out += f"${h.toMapString}%10s"

          case _ => out += "          "
        }

      }
      out += "\n"
      out += "\n"
    }

    out
  }
}




