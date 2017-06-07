def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => y * y :: squareList1(ys)
  }

def squareList(xs: List[Int]): List[Int] =
  xs map(x => x * x)

println(squareList(1::2::3::4::Nil))
println(squareList1(1::2::3::4::Nil))
