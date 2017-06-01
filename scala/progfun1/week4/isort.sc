def isort(xs: List[Int]): List[Int] = xs match {
  case Nil => Nil
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case Nil => x :: Nil
  case y :: ys => if (x < y) x :: xs else y :: insert(x, ys)
}

val list1 = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
val list2 = 10 :: 9 :: 8 :: 7 :: 6 :: 5 :: 0 :: Nil

println(isort(list1))
println(isort(list2))
