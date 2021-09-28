package Integral

case class Simpson[T <: Function](function: T) extends Solver {
  override def method(x1: Double, x2: Double): Double = {
    (x2 - x1) / 6.0 * (function.f(x1) + 4.0 * function.f((x1 + x2) / 2.0) + function.f(x1))
  }
}
