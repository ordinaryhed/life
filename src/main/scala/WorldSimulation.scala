object WorldSimulation extends App {
  (0 to 4).foreach(y => {
    (0 to 10).foreach(x => {
      World.apply(x, y, Set(new Wood(y), new FreshWater(x)), Set(new AnimalGroup("Human", 1)))
    })
  })

  println(World.toConsole)

  (0 to 3).foreach(c => {
    (0 to 3).foreach(w => World.tick())

    println(World.toConsole)
  })
}
