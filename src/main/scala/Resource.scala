abstract class Resource {
  def name():String = {"Resource"}
  def lev():Int = {0}
  def food():Int = {0}
  def tickResource(r:Set[Resource]):Set[Resource]
}


class HexResources {
  var resources = Map[String, Resource]()

  def add(resource: Resource):HexResources = ???

  def add(resource: HexResources):HexResources = ???
}

case class Wood(level:Int = 1) extends Resource {
  override def name(): String = "Wood"
  override def food(): Int = {2}
  override def lev(): Int = {level}

  def tickResource(existingResources:Set[Resource]):Set[Resource] = {
    existingResources.collect {
      case a: FreshWater => if (a.level > level) {
        return existingResources
          .filter(a => (!a.name().equals("Wood"))) + new Wood(level +  1)
      }
    }

    existingResources
  }
}


case class FreshWater(level:Int = 1) extends Resource {
  override def name(): String = "FreshWater"
  override def lev(): Int = {level}
  def tickResource(r:Set[Resource]):Set[Resource] = {
    var hasWood:Boolean = false;
    r.collect {
      case a: Wood =>  hasWood = true
    }

    if (!hasWood) {
      return Set(new Wood(1), this)
    }

    Set(this)
  }
}


case class Flint( val level:Int = 1) extends Resource {
  override def name(): String = "Flint"
  def tickResource(r:Set[Resource]):Set[Resource] = {Set(this)}
}


case class Grain( val level:Int = 1) extends Resource {
  override def name(): String = "Grain"
  def tickResource(r:Set[Resource]):Set[Resource] = {Set(this)}
}


case class Soil( val level:Int = 1) extends Resource {
  override def name(): String = "Soil"
  def tickResource(r:Set[Resource]):Set[Resource] = {Set(this)}
}
