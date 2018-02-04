import World.world

import scala.util.Random

case class Hex(resources: Set[Resource], animals: Set[AnimalGroup]) {
  def getAdjecentHexes(hex: Hex) = {

  }

  def availableFoodFromNature(): Int = {
    resources
      .map(r => r.food())
      .sum
  }


  // 6 x 3
  def hexToConsole(x:Int, y:Int, row:Int):String = {
    val tot = resources.map(r => r.lev()).sum
    var colours = Map[(Int, Int), String]() // .withDefaultValue(Console.BLACK + Console.WHITE + " ")
    val ran = new Random(x + 13*y)

    // resources
    resources.foreach(rr => {
      for( x <- 0 until rr.lev()*2) {
        colours += ((ran.nextInt(6), ran.nextInt(3)) -> (rr.colour() + " "))
      }
    })

    // animals
    animals.foreach(animal => {
      if (animal.gSize > 9) {
        colours += (2, 1) -> (Console.RED_B + Console.BLACK + animal.gSize / 10)
        colours += (3, 1) -> (Console.RED_B + Console.BLACK + animal.gSize % 10)
      }
      else {
        colours += (2, 1) -> (Console.RED_B + Console.BLACK + " ")
        colours += (3, 1) -> (Console.RED_B + Console.BLACK + animal.gSize)
      }
    })


    Range(0, 6).map(x => {
      if (colours.contains((x, row)))
        colours.get((x, row)) match {
          case Some(colour) => colour
          case None => Console.BLACK  + " "
        }
      else Console.BLACK  + " "
    }).mkString("")
  }

  def resourcesMapString():String = {
    resources
      .map(r => r.colouredOutput())
      .mkString(" ")
  }

  def animalsMapString():String = {
    animals
      .map(r => "" + r.rece.charAt(0) + r.gSize)
      .mkString(" ")

  }
}


object World {
  var world : Map[(Int, Int), Hex] = Map.empty

  def apply(x:Int, y:Int, resource: Set[Resource] = Set(), animals:Set[AnimalGroup] = Set()) : Hex = {
    val land = new Hex(resource, animals)
    world += ((x,y) -> land)
    land
  }

  def findHex(x:Int, y:Int):Option[Hex] = {
    world.get(x,y)
  }

  def resources(): Set[Resource] = {
    val zeroToTwo:Int = Random.nextInt(3)
    if (zeroToTwo.equals(0)) {
      Set(new FreshWater(3))
    }
    else if (zeroToTwo == 1) {
      Set(new Wood(1), new FreshWater(3))
    }
    else  {
      Set(new Wood(1))
    }
  }

  def tick(): Unit = {
    world.collect({
      case ((x: Int, y:Int), h: Hex) =>  {
        val rs = h.resources
          .collect({case (r: Resource) =>  r.tickResource(h.resources)})
          .flatten
        /*
                val as = h.animals
                  .collect({case (r: AnimalGroup) =>  r.tick(h.availableFoodFromNature())})

                val hs = h.animals
                  .collect({case (a: AnimalGroup) =>  a.move(x,y)})
        */
        World.apply(x, y, rs, h.animals)
      }
    })
  }

  override def toString: String = {
    val minX:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (x) }).min
    val maxX:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (x) }).max
    val minY:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (y) }).min
    val maxY:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (y) }).max

    var worldInAscii = ""
    worldInAscii += "\n"
    worldInAscii += "          "

    for( x <- minX until maxX+1 ) {
        worldInAscii += f"${"(" + x + ")"}%10s"
    }
    worldInAscii += "\n"

    for( y <- minY until maxY+1 ) {
      worldInAscii += "\n"
      worldInAscii += f"${"(" + y + ")"}%10s"

      // Resources
      for( x <- minX until maxX+1 ) {
        findHex(x, y) match {
          case Some(h) =>
            worldInAscii += f"${h.resourcesMapString()}%10s"
          case _ => worldInAscii += "          "
        }
      }
      worldInAscii += "\n"
      worldInAscii += "          "
      // Animals
      for( x <- minX until maxX+1 ) {
        findHex(x, y) match {
          case Some(h) =>
            worldInAscii += f"${h.animalsMapString()}%10s"

          case _ => worldInAscii += "          "
        }
      }
      worldInAscii += "\n"
      worldInAscii += "          "

      worldInAscii += "\n"
    }

    worldInAscii
  }

  def toConsole: String = {
    val minX:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (x) }).min
    val maxX:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (x) }).max
    val minY:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (y) }).min
    val maxY:Int = world.collect({ case ((x: Int, y:Int), z: Hex) => (y) }).max

    val HEX_GAP = Console.BLACK + "|"
    val INDENT = Console.BLACK + "      "
    val BR = Console.BLACK + "\n"


    var worldInAscii = BR

    // x - numbers
    worldInAscii += INDENT
    for( x <- minX until maxX+1 ) {
      worldInAscii += f"${"(" + x + ")"}%6s" + HEX_GAP
    }
    worldInAscii += "" + BR

    // rows
    for( y <- minY until maxY+1 ) {
      // y-number
      worldInAscii += BR
      worldInAscii += f"${"(" + y + ")"}%6s"

      // the hexes
      for (row <- Range(0,3)) {
        for( x <- minX until maxX+1 ) {
          findHex(x, y) match {
            case Some(h) =>
              worldInAscii += f"${h.hexToConsole(x, y, row)}%6s"+ HEX_GAP

            case _ => worldInAscii += "" + INDENT + HEX_GAP
          }
        }
        worldInAscii += BR
        worldInAscii += INDENT
      }
      worldInAscii += Range(minX*6, maxX*6+1).map(n => "--").mkString("")

    }

    worldInAscii
  }
}




