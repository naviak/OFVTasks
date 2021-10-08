package Cauchy

import breeze.linalg.{DenseVector, linspace}
import breeze.numerics.exp
import breeze.plot.{Figure, plot}

object RungeKutta extends App {
  def dxdt(iX: Double): Double =
  {
    -iX
  }
  def log2(iX: Double): Double = {
    math.log10(math.abs(iX) + 1e-15) / math.log10(2.0)
  }
  def euler_method(xAxis: DenseVector[Double], initVal: Double): DenseVector[Double] = {
    val step = xAxis(1) - xAxis(0)
    lazy val y_res: LazyList[Double] = initVal #:: y_res.map(t => t + dxdt(t) * step)
    DenseVector(y_res.take(xAxis.size).toArray)
    }
  def rungeKutta_method2(xAxis: DenseVector[Double], initVal: Double): DenseVector[Double] ={
    val a = 0.5
    val h = xAxis(1) - xAxis(0)
    lazy val y_res: LazyList[Double] = initVal #:: y_res.map(t => t + h * ((1 - 1 / (2 * a))*dxdt(t) + 1 / (2 * a) *dxdt(t + a * h*dxdt(t))))
    DenseVector(y_res.take(xAxis.size).toArray)
  }
  def rungeKutta_method4(xAxis: DenseVector[Double], initVal: Double): DenseVector[Double] ={
    val h = xAxis(1) - xAxis(0)
    lazy val y_res: LazyList[Double] = initVal #:: y_res.map(t => {
      val k1 = dxdt(t);
      val k2 = dxdt(t + h / 2 * k1);
      val k3 = dxdt(t + h / 2 * k2);
      val k4 = dxdt(t + h * k3)
      t + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)})
    DenseVector(y_res.take(xAxis.size).toArray)
  }
  val x_grid = linspace(0, 3, 20)
  val y_grid = exp(-x_grid)
  val y_kutta = rungeKutta_method2(x_grid, 1.0)
  val y_kutta4 = rungeKutta_method4(x_grid, 1.0)
  val y_euler = euler_method(x_grid, 1.0)
  val fig = Figure()
  val p = fig.subplot(0)

  p += plot(x_grid,y_grid)
  p += plot(x_grid,y_kutta,name = "runge-kutta",style = '.')
  p += plot(x_grid,y_euler,name = "euler", style = '.')
  p += plot(x_grid,y_kutta4,name = "kutta 4", style = '.')
  p.legend = true
  p.setXAxisDecimalTickUnits()
  p.setYAxisDecimalTickUnits()
  fig.refresh

  val fig2 = Figure()
  val p2 = fig2.subplot(0)
  p2.legend = true
  p2.title = "Error"
  p2 += plot(x_grid, (y_euler - y_grid).map(t => log2(t)), name = "Euler")
  p2 += plot(x_grid, (y_kutta - y_grid).map(t => log2(t)), name = "Runge")
  p2 += plot(x_grid, (y_kutta4 - y_grid).map(t => log2(t)), name = "Runge 4")
  p2.setXAxisDecimalTickUnits()
  p2.setYAxisDecimalTickUnits()
  fig2.refresh

}
