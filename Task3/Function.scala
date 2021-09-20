package Integral


import breeze.linalg._
import breeze.plot._
import breeze.optimize._

trait Function {
  def f(x: Double): Double

  val x: DenseVector[Double]

  def plotItself: Unit = {
    val fig = Figure()
    val p = fig.subplot(0)
    p += plot(x, x.map(t => f(t)))
    fig.refresh
  }
}

case class inv1plusSquared(a: Double, b: Double, count: Int) extends Function {
  val x = linspace(a, b, count)

  override def f(_x: Double): Double = {
    1 / (1 + _x * _x)
  }

}

case class expSin(a: Double, b: Double, count: Int) extends Function {
  val x = linspace(a, b, count)

  override def f(_x: Double): Double = {
    _x.sign * math.pow(math.abs(_x), 1.0 / 3.0) * math.exp(math.sin(_x))
  }
}
