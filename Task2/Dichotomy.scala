import QuantEquation.ChrestomaticEquation

import scala.annotation.tailrec

final class Dichotomy extends Method {
  val fa = ChrestomaticEquation.f(ChrestomaticEquation.start)

  def solve(x0: Double, f: (Double) => Double, eps: Double): Double = {
    @tailrec
    def bis(a: Double, b: Double, eps: Double, iter: Int): Double = {
      val m = (a + b) / 2
      val fm = ChrestomaticEquation.f(m)
      if (fm.abs < eps) {
        println(s"find root for $iter iterations")
        return m
      }
      if (fm.sign != fa.sign)
        bis(a, m, eps, iter + 1)
      else bis(m, b, eps, iter + 1)
    }

    bis(ChrestomaticEquation.start, ChrestomaticEquation.end, eps, 0)
  }
}