var map : Map[(Int, Int), String] = Map.empty

map = map + ((1,2) -> "a")
map = map + ((2,1) -> "b")
map = map + ((3,4) -> "v")

map.collect({ case ((x: Int, y:Int), z: String) => (x) }).min

val x = 2
for( y <- 4 until 5){
  for( x <- 5 until 11){
    print(s"($x,$y) ")
  }
  println
}

val l = List(1,"BBBB",true)
l.map(a => a)


l.map(r => r.getClass.getSimpleName.charAt(0)).mkString("")


var s = Set("a", "b", "c")
s.filter(a => (!a.equals("a")))

