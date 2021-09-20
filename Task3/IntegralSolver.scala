package Integral

import breeze.linalg._
import breeze.plot._
import breeze.optimize._

object IntegralSolver extends App{
  object Helper{

    private val x = linspace(0.0, 3 * math.Pi,6)

    def integrate[T <: Solver](method: T) = {
      val ls = x.toArray.toList
      method.calculate(ls)
    }

    def plotIntegral[T <: Solver](method: T): Unit = {
      val fig = Figure()
      val p = fig.subplot(0)
      p += plot(x,x.map(t => method.f(t)))

      fig.refresh
    }
  }
  val trap = Trapezoidal(math.sin)
  println(Helper.integrate(trap))
  Helper.plotIntegral(trap)
  val simp = Simpson(math.sin)
  println(Helper.integrate(simp))


}
