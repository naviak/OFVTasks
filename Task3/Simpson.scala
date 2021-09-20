package Integral

case class Simpson(f: Double => Double) extends Solver {
  override def method(x1: Double, x2: Double): Double = {
    (x2 - x1) / 6.0 * (f(x1) + 4.0 * f((x1 + x2) / 2.0) + f(x1))
  }
}
