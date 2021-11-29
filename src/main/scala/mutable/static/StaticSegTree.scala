package mutable.static

import scala.reflect.ClassTag

class StaticSegTree[T: Numeric: ClassTag](_n: Int, fx: (T, T) => T, ex: T) {
  var n = 1
  while (n < _n) n *= 2
  val data = new Array[T](2 * n - 1)

  def update(_i: Int, v: T): Unit = {
    var i = _i + n - 1
    data(i) = v
    while (0 < i) {
      i = (i - 1) / 2
      data(i) = fx(data(i * 2 + 1), data(i * 2 + 2))
    }
  }

  def query(a: Int, b: Int): T = query(a, b, 0, 0, n)
  def query(a: Int, b: Int, k: Int, l: Int, r: Int): T = {
    if (r <= a || b <= l) ex
    else if (a <= l && r <= b) data(k)
    else {
      val left = query(a, b, k * 2 + 1, l, l + (r - l) / 2)
      val right = query(a, b, k * 2 + 2, l + (r - l) / 2, r)
      fx(left, right)
    }
  }
}
