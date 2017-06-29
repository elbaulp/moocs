package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Arbitrary.arbitrary

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

}
