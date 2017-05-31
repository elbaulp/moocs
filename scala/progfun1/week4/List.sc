trait List[+T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
  override def toString = head + " :: " +  tail.toString
}

object Nil extends List[Nothing] {
  override def isEmpty = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString = "Nil"
}

object List {
  //def List[T]:List[T] = new Nil[T]
  //def List[T](that:T):List[T] = new Cons(that, new Nil)
  //def List[T](that:T, that2:T) = new Cons(that, new Cons(that2, new Nil))
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, Nil))
  def apply[T](x: T): List[T] = new Cons(x, Nil)
  def apply[T]() = Nil
}

val empty = List()
println(empty)
val oneElem = List("Hello")
println(oneElem)
val twoElem = List("Hello", "World")
println(twoElem)

val x: List[String] = Nil

val y: List[String] = List("Hello", "World")
println(y.prepend(0))

def f(xs: List[String], x: Int) = xs prepend x

// Because List[+T] is covariant and prepend is lower bounded on T, the following works becasuse
// String and Int have a common parent (Any): Int >: String = Any, and as List[+T] is covariant
// in T, List[String] <: List[Any]
println(f(y, 10))
