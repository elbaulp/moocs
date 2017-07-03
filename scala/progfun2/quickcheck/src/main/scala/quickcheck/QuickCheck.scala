package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Arbitrary.arbitrary
import scala.annotation.tailrec

/**
  * - A property is the testable unit in ScalaCheck.
  * - forAll, ... defined in Prop
  * - forAll takes a function and return Boolean or other Prop.
  * - Implication: ==>, f.e: (n >= 0 && n < 10000) ==> (List.fill(n)("").length == n)
  * - Properties can be combined: p1 && p2...
  * - and grouped
  */

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  val genEmpty = const(empty)

  lazy val genHeap: Gen[H] = oneOf(
    genEmpty,
    for {
      i <- arbitrary[Int]
      h <- oneOf(genEmpty, genHeap)
    } yield insert(i, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // If you insert any two elements into an empty heap, finding the minimum of the
  // resulting heap should get the smallest of the two elements back.
  property("2items2emptyH") = forAll { (a: Int, b:Int) =>
    val h = insert(a, empty)
    val h1 = insert(b, h)
    val min = a min b
    findMin(h1) == min
  }

  // If you insert an element into an empty heap, then delete the minimum, the
  // resulting heap should be empty.
  property("itemToEmptyAndRemove") = forAll { a:Int =>
    val h = insert(a, empty)
    val eH = deleteMin(h)
    eH == empty
  }


  // Given any heap, you should get a sorted sequence of elements when
  // continually finding and deleting minima. (Hint: recursion and helper
  // functions are your friends.)
  property("sorted") = forAll { h: H =>
    @tailrec
    def removeAll(h: H, acc:List[A] = Nil): List[A] = {
      if (isEmpty(h)) Nil
      else {
        val min = findMin(h)
        removeAll(deleteMin(h), min :: acc)
      }
    }

    removeAll(h) == removeAll(h).sorted
  }

  // Finding a minimum of the melding of any two heaps should return a
  // minimum of one or the other.
  property("Melting") = forAll { (h1: H, h2: H) =>
    (!isEmpty(h1) && !isEmpty(h2)) ==> {
      val min = findMin(h1).min(findMin(h2))
      findMin(meld(h1, h2)) == min
    }
  }

  // A heap with 2 elements should return first the minimun and
  // then the max
  property("MinMax") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val mi = findMin(h)
    val newH = deleteMin(h)
    val ma = findMin(newH)

    all(
      "min" |: (a min b) == mi,
      "max" |: (a max b) == ma
    )
  }
}
