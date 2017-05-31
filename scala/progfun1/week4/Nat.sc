// Peano numbers
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true
  override def predecessor: Nat = throw new Error("Zero.predecessor")
  override def + (that: Nat): Nat = that
  override def - (that: Nat): Nat =
    if (that.isZero) this
    else throw new Exception("Zero.-")
  override def toString = "Zero"
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false
  override def predecessor: Nat = n
  override def + (that: Nat): Nat = new Succ(n + that)
  override def - (that: Nat): Nat =
    if (that.isZero) this else n - that.predecessor
  override def toString = "Succ -> " + n.toString
}

val one  = new Succ(Zero)
println("One: " + one)

val two = new Succ(new Succ(Zero))
println("Two: " + two)

val three = one + two
println("Three: " + three)

val six = three + three
println("Six: " + six)

println("Two - One: " + (two - one))
println("One - One: " + (one - one))
println("Five: " + (six - one))
println("One - Two: " + (one - two)) // error, result is negative
