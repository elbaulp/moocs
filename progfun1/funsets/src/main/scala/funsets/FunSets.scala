package funsets

import scala.annotation.tailrec


/**
 * 2. Purely Functional Sets.
 */
object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => elem == x

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => s(x) || t(x)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => s(x) && t(x)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = x => s(x) && p(x)

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) true
      else if (s(a)) p(a)
      else iter(a + 1)
    }
    iter(-bound)
  }

  /* def forall(s: Set, p: Int => Boolean): Boolean = {
   *   @tailrec
   *   def iter(a: Int, b: Int): Boolean = {
   *     if (a > b) true
   *     else s(a)  match {
   *       case true => p(a)
   *       case false => iter(a + 1, b)
   *     }
   *   }
   *   iter(-1000, 1000)
   * } */

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def iter(a: Int): Boolean = {
      if (a > bound) false
      else if (s(a) && p(a)) true
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set =
    /**
      * Here we have two anonymous functions: x => ... and inside ... y => f(y) == x
      * substituting the first function: F_x(x) = Exists(s, y => f(y) == x), so
      * what is happening is that for every value we want to map we check if the original
      * value is in the set, and return true there is a value in [-1000, 1000] such that f(y) == x
      * , for example:
      *
      * s = {1,2,3}
      * f(y) = 2y
      *
      * F_x(1) = Exists({1,2,3}, 2y == 1) => False
      * F_x(2) = Exists({1,2,3}, 2y == 2) => True
      *
      * In every step we check in [-1000,1000] if the value is in the set {1,2,3} and check if it
      * has a corresponding mapping (s(a) && p(a) inside exits), so for F_x(1) the second condition
      * (p(a)) never holds because there is no mapping such that 2y == 1. On the other hand, in
      *
      * F_x(2) s(2) is true and p(2) is also true because in [-1000, 1000] exists a value such that
      * 2y == 2, with y = 1.
      */
    x => exists(s, y => f(y) == x)
  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }
}
