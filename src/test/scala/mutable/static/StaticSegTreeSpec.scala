package mutable.static

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams

class StaticSegTreeSpec extends AnyFlatSpec with Diagrams {
  // https://leetcode.com/problems/range-sum-query-mutable/
  def gSum(a: Int, b: Int): Int = a + b
  val segTree = new StaticSegTree[Int](3, gSum, 0)
  segTree.update(0, 1)
  segTree.update(1, 3)
  segTree.update(2, 5)
  "RSQ" should "return sum in the range" in {
    assert(segTree.query(0, 3) === 9)
  }

  def gMin(a: Int, b: Int): Int = a.min(b)
  val minSegTree = new StaticSegTree[Int](4, gMin, Int.MaxValue)
  minSegTree.update(0, 7)
  minSegTree.update(1, 5)
  minSegTree.update(2, 3)
  minSegTree.update(3, 1)
  "RMQ" should "return minimum value in the range" in {
    assert(minSegTree.query(0, 4) === 1)
    assert(minSegTree.query(0, 3) === 3)
    assert(minSegTree.query(0, 2) === 5)
    assert(minSegTree.query(0, 1) === 7)
  }
}
