case class AnimalGroup (rece: String, gSize:Int) {
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
