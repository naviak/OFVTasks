package Bessel

import breeze.linalg.linspace
import breeze.plot.{Figure, plot}


object Bessel extends App {
  trait Solver {
    def function: Double => Double

    val ar: List[Double]

    def calculate(): Double = {
      val arr = ar.zip(ar.tail)
      arr.foldLeft(0.0) { case (acc, x1) => acc + method(x1._1, x1._2) }
    }

    protected def method(x1: Double, x2: Double): Double
  }

  case class Simpson(function: Double => Double, ar: List[Double]) extends Solver {
    override def method(x1: Double, x2: Double): Double = {
      ((x2 - x1) / 6.0) * (function(x1) + 4.0 * function((x1 + x2) / 2.0) + function(x2))
    }
  }

  def besselFunction(x: Double, m: Int): Double = {
    def func(t: Double): Double = {
      math.cos(m * t - x * math.sin(t))
    }

    val xval = linspace(0, math.Pi, 500).toArray.toList
    val calc = Simpson(func, xval)
    calc.calculate() / math.Pi
  }

  def besselFunctionDerivative(x: Double, m: Int): Double = {
    val derivativeStep = 10e-6
    if(x == 0.0){
      val r = (besselFunction(x + derivativeStep, m) - besselFunction(x, m)) / derivativeStep
      r
    }
    if (x == math.Pi){
      val r = (besselFunction(x, m) - besselFunction(x - derivativeStep, m)) / derivativeStep
      r
    }
    else {
      val r = (besselFunction(x + derivativeStep, m) - besselFunction(x - derivativeStep, m)) / (2.0 * derivativeStep)
      r
    }
  }

  var x = 0.0
  var ls:List[Double] = List()
  while (x < math.Pi) {
    println(s"x = $x")
    val result: Double = besselFunctionDerivative(x, 0) + besselFunction(x, 1)
    ls = ls :+ result
    require(result < 1e-10)
    println(result)
    x += 0.1
  }
  val fig = Figure()
  val fig2 = Figure()
  val p = fig.subplot(0)
  val p2 = fig2.subplot(0)
  val xx = linspace(0,20,4000)
  p += plot(xx, xx.map(t => besselFunction(t,0)))
  p += plot(xx,xx.map(t => besselFunction(t,1)))
  p2 += plot(linspace(0.0,math.Pi,ls.length),ls)
  p2.setYAxisDecimalTickUnits()
  fig.refresh
  fig2.refresh
}
