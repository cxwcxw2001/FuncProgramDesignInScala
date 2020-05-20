package quickcheck

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      x <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  }
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (x1: A, x2: A) =>
    val h = insert(x2, insert(x1, empty))
    val m = if (x1 > x2) x1 else x2
    findMin(h) == m
  }

  property("gen2") = forAll { (h: H) =>
    // borrow from https://github.com/Aigloun/coursera-functional-program-design-in-scala/blob/765e739f750070748017012291288b4b909a40c4/quickcheck/src/main/scala/quickcheck/QuickCheck.scala#L46
    def isSorted(heap: H): Boolean = {
      if (isEmpty(heap)) true
      else {
        val newHeap = deleteMin(heap)
        isEmpty(newHeap) || findMin(heap) <= findMin(newHeap) && isSorted(newHeap)
      }
    }
    isSorted(h)
  }

  property("empty1") = forAll { (x: A) =>
    val h = insert(x, empty)
    isEmpty(deleteMin(h))
  }

  property("min2") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val h = meld(h1, h2)
    findMin(h) == (if (min1 < min2) min1 else min2)
  }

}
