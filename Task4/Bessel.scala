package Integral

import breeze.linalg.{DenseVector, linspace}

object Bessel extends App {
  def BesselF(x: Double, m: Int): Double = {
    val func = BesselFunction(x, m)
    val solver = Simpson(func)
    solver.calculate() / math.Pi
  }

  def BesselDerivative(x: Double, m: Int): Double = {
    val derivativeStep: Double = 10e-6
    (BesselF(x + derivativeStep, m) - BesselF(x - derivativeStep, m)) / (2.0 * derivativeStep)
  }

  var x: Double = 0.1
  while (x < 1.1) {
    println(s"x = $x")
    val result: Double = BesselDerivative(x, 0) + BesselF(x, 1)
    println(result)
    x += 0.1
  }
}
