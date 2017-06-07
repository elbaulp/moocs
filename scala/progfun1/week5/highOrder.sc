def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList1(ys)
  }

def squareList(xs: List[Int]): List[Int] =
  xs map(x => x * x)

println(squareList(1::2::3::4::Nil))
println(squareList1(1::2::3::4::Nil))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val spanned = xs span(y => y == x)
    spanned._1 :: pack(spanned._2)
}

println(pack(List("a", "a", "a", "b", "c", "c", "a")))
