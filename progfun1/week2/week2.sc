object exercise2 {
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if(a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }
}

exercise2.sum(x => x * x, 3, 5)

object exerciseCurrying {
  // Non general version
//  def product(f: Int => Int)(a: Int, b: Int): Int = {
//      if (a > b) 1
//      else f(a) * product(f)(a + 1, b)
//  }

  // General case
  private def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b:Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))

  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  // Factorial in terms of product
  def factorial(n: Int): Int = product(x => x)(1, n)


}

exerciseCurrying.product(x => x * x)(3, 4)
exerciseCurrying.factorial(5)




