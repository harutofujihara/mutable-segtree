package mutable.static

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.diagrams.Diagrams

class StaticLazySegTreeSpec extends AnyFlatSpec with Diagrams {
  // https://leetcode.com/problems/range-sum-query-mutable/
  def xSum(a: Int, b: Int): Int = a + b
  def aSum(a: Int, b: Int): Int = b
  def mSum(a: Int, b: Int): Int = b
  def pSum(a: Int, l: Int): Int = a * l

  val segTree = new StaticLazySegTree[Int, Int](3, xSum, aSum, mSum, pSum, 0, 0)
  segTree.update(0, 3, 4)
  "RSQ" should "return sum in the range" in {
    assert(segTree.query(0, 3) === 12)
  }

  def xMin(a: Int, b: Int): Int = a.min(b)
  def aMin(a: Int, b: Int): Int = a.min(b)
  def mMin(a: Int, b: Int): Int = a.min(b)
  def pMin(a: Int, l: Int): Int = a

  val minSegTree =
    new StaticLazySegTree[Int, Int](4,
                                    xMin,
                                    aMin,
                                    mMin,
                                    pMin,
                                    Int.MaxValue,
                                    Int.MaxValue)
  minSegTree.update(0, 4, 4)
  minSegTree.update(1, 2, 2)

  "RMQ" should "return minimum value in the range" in {
    assert(minSegTree.query(0, 4) === 2)
  }
}
