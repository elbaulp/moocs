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
    val (packed, rest) = xs span(y => y == x)
    packed :: pack(rest)
}

println(pack(List("a", "a", "a", "b", "c", "c", "a")))


def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map(ys => (ys.head, ys.size))

println(encode(List("a", "a", "a", "b", "c", "c", "a")))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( (x, y) => f(x) :: y )

println(mapFun(List(1,2,3,4,5), (x:Int) => x + x))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (x, y) => y + 1)

println(lengthFun(List(1,2,3,4)))
