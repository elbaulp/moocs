val xs = List(1,2,3)
val ys = List(1,2,3)

println((for((i, j) <- xs zip ys) yield (i * j)).sum)
