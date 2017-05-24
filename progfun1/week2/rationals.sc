val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)


class Rational(x: Int, y: Int) {
  def add(r: Rational) =
    new Rational(numer * r.denom + r.numer * denom,
      denom * r.denom)

  def mul(r: Rational) = ???

  def neg = new Rational(-numer, y)

  def sub(that: Rational) = add(that.neg)

  override def toString = numer + "/" + denom

  def numer = x

  def denom = y
}
x.neg

x.sub(y).sub(z)