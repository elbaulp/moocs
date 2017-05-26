package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val s4 = singletonSet(1)

    val u12 = union(s1, s2)
    val u123 = union(u12, s3)
    val u1234 = union(u123, s4)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection contains common values in the sets") {
    new TestSets {
      val s = intersect(s1, s2)
      val ss = intersect(s1, s4)
      assert(!contains(s, 1), "Intersect 1")
      assert(contains(ss, 1), "Intersect 2")
    }
  }

  test("Difference contains all elements in S1 not in S2 ") {
    new TestSets {
      val u = union(s1, s2)
      val u2 = union(u, s3)
      val d = diff(u2, s4)

      assert(contains(d, 2), "Diff contains 2")
      assert(contains(d, 3), "Diff contains 3")
      assert(!contains(d, 1), "Diff not contains 1")
    }
  }

  test("Filter contains all elements for which p holds") {
    new TestSets {
      val fil = filter(u1234, x => x > 2)

      assert(contains(fil, 3), "Filter contains 3")
      assert(!contains(fil, 2), "Filter does not contains 2")
    }
  }

  test("P holds for all elements in the range [-1000,1000]") {
    new TestSets {
      val f = forall(u1234, x => x < 5)
      val f2 = forall(u1234, x => x < 0)

      assert(f == true, "All values in {1,2,3} < 5")
      assert(f2 == false, "All values in {1,2,3} > 0")
    }
  }

  test("P holds if there exists at least one elements that satisfies p") {
    new TestSets {
      val f = exists(u1234, x => x % 2 == 0)
      val f1 = exists(u1234, x => x < 0)

      assert(f == true, "At least on element in {1,2,3} is multiple of 2")
      assert(f1 == false, "No element in {1,2,3} < 0")
    }
  }
}
