trait List[T] {
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty = false
  override def toString = head + " :: " +  tail.toString
}

class Nil[T] extends List[T] {
  override def isEmpty = true
  override def head: Nothing = throw new NoSuchElementException("Nil.head")
  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  override def toString = "Nil"
}

object List {
  //def List[T]:List[T] = new Nil[T]
  //def List[T](that:T):List[T] = new Cons(that, new Nil)
  //def List[T](that:T, that2:T) = new Cons(that, new Cons(that2, new Nil))
  def apply[T](x: T, y: T): List[T] = new Cons(x, new Cons(y, new Nil))
  def apply[T](x: T): List[T] = new Cons(x, new Nil)
  def apply[T]() = new Nil
}

val empty = List()
println(empty)
val oneElem = List("Hello")
println(oneElem)
val twoElem = List("Hello", "World")
println(twoElem)
