case class AnimalGroup (rece: String, gSize:Int) {
/*
  def move(x:Int, y:Int): AnimalGroup = {
    val a = List((x-1) -> (y-1),
      (x) -> (y-1),
      (x+1) -> (y-1),
      (x-1) -> (y),
      (x+1) -> (y),
      (x-1) -> (y+1),
      (x) -> (y+1),
      (x+1) -> (y+1))


    val hex2: Seq[(Int, Int, Int)] = a.map(xy => {(xy, World.findHex(xy._1, xy._2).get)})
      .collect({
        case ((xx:Int, yy:Int), h:Hex) => {
          (xx, yy, attractedBy(h))
        }
      })

    val hex3 = hex2.reduceLeft((a,b) => {if (a._2 > b._2) { (a)} else {b}})

    println("max: = " + hex3)
    //      .map(xyhex => {println(xyhex._1 + " " + xyhex._2)})
//    val hex3 = hex2.sortBy(h =>

//
//    {
//      Some(h):
//    }).sortBy(h => attractedBy(h)).last
//    println(hex2)

    this.copy(x = hex3._1, y = hex3._2)
  }
*/
  def attractedBy(hex:Hex): Int = {
    hex.resources
      .collect({
        case w:Wood => w.level
        case f:FreshWater => f.level
      })
      .sum
  }

  def foodNeeded() :Int = {
    gSize
  }

  def tick(foodAvailable:Int) :AnimalGroup = {
    val gs = gSize
    if (foodNeeded() < foodAvailable) {
      return this.copy(gSize = gs+1)
    }
    else if (foodNeeded() > foodAvailable) {
      return this.copy(gSize = gs-1)
    }
    this
  }
}
