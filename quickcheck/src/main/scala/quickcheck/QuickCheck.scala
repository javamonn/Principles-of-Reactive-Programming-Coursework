package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    findMin(insert(a, empty)) == a
  }

  property("min2") = forAll { (int1: Int, int2: Int) =>
    val min = Math.min(int1, int2)
    var h = insert(int1, empty)
    h = insert(int2, h)
    findMin(h) == min
  }

  property("minMeld") = forAll { (h1: H, h2: H) => 
    val min = Math.min(findMin(h1), findMin(h2))
    val h = meld(h1, h2)
    findMin(h) == min
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMin1") = forAll { (a: Int) =>
    var h = insert(a, empty)
    h = deleteMin(h)
    isEmpty(h) == true
  }

  property("deleteMin2") = forAll { (h: H) =>
    def loop(h: H, ls: List[Int]): List[Int] = {
      if(isEmpty(h)) {
        ls
      } else {
        loop(deleteMin(h), findMin(h) :: ls)
      }
    }
    def sorted(l: List[Int]): Boolean = l.view.zip(l.tail).forall(x => x._1 <= x._2)
    val ls = loop(h, List.empty).reverse
    sorted(ls) == true
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
