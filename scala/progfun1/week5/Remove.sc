def removeAt[T](n: Int, xs: List[T]) = xs.take(n) ::: xs.drop(n + 1)

println(removeAt(1, List('a', 'b', 'c', 'd'))) // List(a, c, d)

def flatten(xs: List[Any]): List[Any] = xs match {
  case (y: List[Any]) :: rest =>
    println("XS: " + xs)
    println("\tHead: " + y + " Rest: " + rest)

    flatten(y) ::: flatten(rest)
  case y :: rest =>
    println("\ty :: rest: " + y + " :: " + rest)
    y +: flatten(rest)
  case _ =>
    println("\t_ " + xs)
    xs
}

println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
