package Integral

case class Trapezoidal(f: Double => Double) extends Solver {
  override def method(x1: Double, x2: Double): Double = {
    math.abs(x2-x1) * (f(x1) + f(x2)) / 2.0
  }
}
