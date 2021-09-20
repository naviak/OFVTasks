package Integral
trait Solver{
  def f: Double => Double
  def calculate(ar: List[Double]): Double = {
    val arr = ar.zip(ar.tail)
    arr.foldLeft(0.0){ case (acc, x1) => acc + method(x1._1, x1._2)}
  }
  protected def method(x1: Double, x2: Double): Double
}
