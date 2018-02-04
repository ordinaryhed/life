val x = 10;
val y = 10;


val a = List((x-1) -> (y-1),
  (x) -> (y-1),
  (x+1) -> (y-1),
  (x-1) -> (y),
  (x+1) -> (y),
  (x-1) -> (y+1),
  (x) -> (y+1),
  (x+1) -> (y+1))


val b = a map (xy => {
  xy._1 + xy._2
})
