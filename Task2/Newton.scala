import scala.annotation.tailrec

class Newton extends Method with PhiMethod {

  def solve(x0: Double, f: (Double) => Double, eps: Double): Double = {
    @tailrec def findRoot(x0: Double, f: Double => Double, eps: Double, iter: Int): Double = {
      val x1 = phi(x0, f, eps)
      if (math.abs(x1 - x0) < eps) {
        println(s"find root for $iter iterations")
        x1
      }
      else findRoot(x1, f, eps, iter + 1)
    }

    findRoot(x0, f, eps, 0)
  }

  def phi(x: Double, f: Double => Double, eps: Double): Double = {
    x - f(x) / Utils.derivative(f, x, eps)
  }
}