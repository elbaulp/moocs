package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    print(balance(":-()".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == r || c == 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)


  /**
   * Exercise 2
    */

  def balance(chars: List[Char]): Boolean =  {
    def check(b: Boolean, c: List[Char]): Boolean = c match {
      case head :: tail =>
        println(c)
        if (head == ')' && !b) check(true, tail)
        if (head == '(' && b) check(false, tail)
        else check(b, tail)
      case _ =>
        println(b)
        b
    }
    check(false, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
