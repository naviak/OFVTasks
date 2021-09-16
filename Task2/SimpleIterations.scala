final class SimpleIterations extends Newton {
  override def phi(x: Double, f: Double => Double, eps: Double): Double = {
    x + 0.01 * f(x)
  }
}