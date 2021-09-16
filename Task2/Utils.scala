object Utils {
  def derivative(f: Double => Double, x: Double, eps: Double = 0.00001): Double = {
    (f(x + eps) - f(x)) / eps
  }
}