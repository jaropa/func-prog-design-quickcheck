package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      priority <- arbitrary[Int]
      heap <- oneOf(const(empty), genHeap)
    } yield insert(priority, heap)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("smallestOfTwoElements") = forAll { (elemA: Int, elemB: Int) =>
    findMin(insert(elemA, insert(elemB, empty))) == Math.min(elemA, elemB)
  }

  property("insertDeleteElement") = forAll { elem: Int =>
    deleteMin(insert(elem, empty)) == empty
  }

  property("orderedElements") = forAll { (h: H) =>
    val heapElements = getHeapElements(h, Seq())
    heapElements.sorted(ord) == heapElements
  }

  property("minimumOfTwo") = forAll { (hA: H, hB: H) =>
    if (isEmpty(hA) && isEmpty(hB)) meld(hA, hB) == empty
    else if (isEmpty(hA)) findMin(meld(hA, hB)) == findMin(hB)
    else if (isEmpty(hB)) findMin(meld(hA, hB)) == findMin(hA)
    else {
      val minOfTwo = Math.min(findMin(hA), findMin(hB))
      findMin(meld(hA, hB)) == minOfTwo
    }
  }

  property("meld") = forAll { (hA: H, hB: H, hC: H) =>
    getHeapElements(meld(meld(hA, hB), hC), Nil) == getHeapElements(meld(hA, meld(hB, hC)), Nil)
  }

  def getHeapElements(h: H, elements: Seq[Int]): Seq[Int] = {
    if (isEmpty(h)) elements
    else {
      val task = findMin(h)
      getHeapElements(deleteMin(h), elements :+ task)
    }
  }
}
