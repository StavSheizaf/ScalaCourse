package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      i <- Arbitrary.arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(i, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minOfTwo") = forAll { (m: Int, n: Int) =>
    findMin(insert(m, insert(n,empty))) == Math.min(m,n)
  }

  property("deleteOnlyItem") = forAll { (n: Int) =>
    isEmpty(deleteMin(insert(n, empty)))
  }

  property("sortedDeleted") = forAll { (items: List[Int]) =>
    def sortDeleted(h:H):List[A] =
      if(isEmpty(h)) Nil
      else
        findMin(h)::sortDeleted(deleteMin(h))

    val h = items.foldLeft(empty)((acc, n)=>insert(n, acc))
    sortDeleted(h) == items.sorted
  }


  property("deleteMin") = forAll { (h: H) =>
    if isEmpty(h) then true else
      def min = findMin(h)
      def nextHeap = deleteMin(h)
      isEmpty(nextHeap) || findMin(nextHeap) >= min
  }

  property("deleteMinOfTwoWithEquals") = forAll { (n: Int, m: Int) =>
    def h = insert(m, insert(n, empty))

    if findMin(deleteMin(h)) == n then n >= m else m >= n
  }

  property("deleteMinOfTwo") = forAll { (n: Int, m: Int) =>
    if(n == m) then true else
      def h = insert(m, insert(n, empty))

      if findMin(deleteMin(h)) == n then n > m else m > n
  }

  property("minOfTwoHeaps") = forAll { (h1: H, h2: H) =>
    (h1, h2) match {
      case (Nil, Nil) => isEmpty(meld(h1, h2))
      case (Nil, ts) => findMin(meld(h1, h2)) == findMin(ts)
      case (ts, Nil) => findMin(meld(h1, h2)) == findMin(ts)
      case (t1, t2) => findMin(meld(t1, t2)) == Math.min(findMin(t1), findMin(t2))
    }
  }










