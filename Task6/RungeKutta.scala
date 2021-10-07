package Cauchy

import breeze.linalg.{DenseVector, linspace}
import breeze.numerics.exp
import breeze.plot.{Figure, plot}

object RungeKutta extends App {
  def dxdt(iX: Double): Double =
  {
    -iX
  }
  def euler_method(xAxis: DenseVector[Double], initVal: Double): List[Double] = {
      val step = xAxis(1) - xAxis(0)
      lazy val y_res: LazyList[Double] = initVal #:: y_res.map(t => t + dxdt(t) * step)
      y_res.take(xAxis.size).toList
    }
  def rungeKutta_method2(xAxis: DenseVector[Double], initVal: Double): List[Double] ={
    val a = 0.5
    val h = xAxis(1) - xAxis(0)
    lazy val y_res: LazyList[Double] = initVal #:: y_res.map(t => t + h * ((1 - 1 / (2 * a))*dxdt(t) + 1 / (2 * a) *dxdt(t + a * h*dxdt(t))))
    y_res.take(xAxis.size).toList
  }
  val x_grid = linspace(0, 3, 16)
  val y_grid = exp(-x_grid)
  val y_calc = rungeKutta_method2(x_grid, 1.0)
  val fig = Figure()
  val p = fig.subplot(0)
  p += plot(x_grid,y_grid)
  p += plot(x_grid,y_calc)
  p.setXAxisDecimalTickUnits()
  p.setYAxisDecimalTickUnits()
}
