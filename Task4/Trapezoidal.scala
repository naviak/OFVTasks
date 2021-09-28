package Integral

case class Trapezoidal[T <: Function](function: T) extends Solver {
  override def method(x1: Double, x2: Double): Double = {
    math.abs(x2-x1) * (function.f(x1) + function.f(x2)) / 2.0
  }
}
