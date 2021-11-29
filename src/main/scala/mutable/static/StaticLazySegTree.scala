package mutable.static

import scala.reflect.ClassTag

class StaticLazySegTree[T: Numeric: ClassTag, U: Numeric: ClassTag](
    _n: Int,
    fx: (T, T) => T,
    fa: (T, U) => T,
    fm: (U, U) => U,
    fp: (U, Int) => U,
    ex: T,
    em: U) {
  var n = 1
  while (n < _n) n *= 2
  val data = new Array[T](2 * n - 1)
  val lazyData = new Array[U](2 * n - 1)
  for (i <- 0 until data.length) {
    data(i) = ex
    lazyData(i) = em
  }

  def eval(k: Int, len: Int): Unit = {
    if (lazyData(k) == em) return
    if (k < n - 1) {
      lazyData(k * 2 + 1) = fm(lazyData(k * 2 + 1), lazyData(k))
      lazyData(k * 2 + 2) = fm(lazyData(k * 2 + 2), lazyData(k))
    }
    data(k) = fa(data(k), fp(lazyData(k), len))
    lazyData(k) = em
  }
  def update(a: Int, b: Int, x: U, k: Int, l: Int, r: Int): Unit = {
    eval(k, r - l)
    if (a <= l && r <= b) {
      lazyData(k) = fm(lazyData(k), x)
      eval(k, r - l)
    } else if (a < r && l < b) {
      update(a, b, x, k * 2 + 1, l, l + (r - l) / 2)
      update(a, b, x, k * 2 + 2, l + (r - l) / 2, r)
      data(k) = fx(data(k * 2 + 1), data(k * 2 + 2))
    }
  }
  def update(a: Int, b: Int, x: U): Unit = update(a, b, x, 0, 0, n)

  def query(a: Int, b: Int, k: Int, l: Int, r: Int): T = {
    eval(k, r - l)
    if (r <= a || b <= l) ex
    else if (a <= l && r <= b) data(k)
    else {
      val vl = query(a, b, k * 2 + 1, l, l + (r - l) / 2)
      val vr = query(a, b, k * 2 + 2, l + (r - l) / 2, r)
      fx(vl, vr)
    }
  }

  def query(a: Int, b: Int): T = query(a, b, 0, 0, n)
}
