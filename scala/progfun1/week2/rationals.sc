val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)


class Rational(x: Int, y: Int) {
  require(y != 0, "Denominator must be nonzero")

  def this(x:Int) = this(x, 1)

  private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g

  def <(that: Rational) = numer * that.denom < that.numer * denom

  def max(that:Rational) = if (this < that) that else this

  def mul(r: Rational) = ???

  def - (that: Rational) = this + -that

  def + (r: Rational) =
    new Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)

  def unary_- :Rational = new Rational(-numer, y)

  override def toString = numer + "/" + denom

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

}

x
x - y - z
y + y
x < y
x max y
