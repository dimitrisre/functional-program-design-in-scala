package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for{
      element <- arbitrary[A]
      heap <- frequency((1, Gen.const(empty)), (9, genHeap))
    } yield insert(element, heap)
  }


  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("insert any two elements into an empty heap and get the min should eq the min of 2 elements") = forAll{
    (a: A, b: A) =>
      val min = ord.min(a, b)
      val h = insert(b, insert(a, empty))

      findMin(h) == min
  }

  property("insert an element into an empty heap, then delete the minimum should result in empty heap") = forAll{
    a: A =>
      val h = insert(a, empty)
      deleteMin(h) == empty
  }

  property("Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima") =
    forAll{
      h: H =>
        def loop(h: H): Boolean = {
          if(isEmpty(h)) true
          else{
            val min = findMin(h)
            val h1 = deleteMin(h)
            isEmpty(h1) || (min <= findMin(h1) && loop(h1))
          }
        }

        loop(h)
  }

  property("Finding a minimum of the melding of any two heaps should return a minimum of one or the other") =
    forAll{
      (h1: H, h2: H) =>
        val min = findMin(meld(h1, h2))

        min == findMin(h1) || min == findMin(h2)
    }

  property("The minimum of minimum of 2 heaps should be the same as if the element is moved from the one to the other") =
    forAll{
      (h1: H, h2: H) =>
        val min = ord.min(findMin(h1), findMin(h2))
        min == findMin(meld(deleteMin(h1), insert(min, h2)))
    }

  property("Melding h1, h2 and then melding with h3 should be equal to melding h1 and then h2, h3") =
    forAll{
      (h1: H, h2: H, h3: H) =>
        val h4 = meld(meld(h1, h2),h3)
        val h5 = meld(h1, meld(h2,h3))

        isEqual(h4, h5)
    }

  def isEqual(h1: H, h2: H): Boolean = {
    if(isEmpty(h1) && isEmpty(h2)) true
    else
      findMin(h1) == findMin(h2) && isEqual(deleteMin(h1), deleteMin(h2))
  }
}
