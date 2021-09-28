package Integral

import breeze.linalg._
import breeze.plot._
import breeze.optimize._

object IntegralSolver extends App{
  object Helper{
    def integrate(rightAnswer: Double) = {
      var log2 = (x: Double) => math.log10(x)/math.log10(2.0)
      lazy val lz: LazyList[Int] =  4 #:: lz.map(f => f * 2)
      val y: List[Double] = lz.takeWhile(e => e < math.pow(2,13)).toList.map(f => {
        val func = inv1plusSquared(-1,1,f)
        //val func = expSin(0,1,f)
        //val method = Trapezoidal(func)
        val method = Simpson(func)
        val s = rightAnswer - method.calculate()
        val k = log2(math.abs(s))
        k
      })
      val y1: List[Double] = lz.takeWhile(e => e < math.pow(2,13)).toList.map(f => {
        val func = inv1plusSquared(-1,1,f)
        //val func = expSin(0,1,f)
        val method = Trapezoidal(func)
        //val method = Simpson(func)
        val s = rightAnswer - method.calculate()
        log2(math.abs(s))
      })
      val x = linspace(0.0, y.length,y.length)
      val x1 = linspace(0.0, y.length,y.length)
      val fig = Figure()
      val p = fig.subplot(0)
      p += plot(x,y)
      p += plot(x1,y1)
      println("k for trap " + (y1.last - y1(0))/y1.length)
      println("k for simpson " + (y.last - y(0))/y.length)
      fig.refresh
    }
  }

  val f1 = inv1plusSquared(-1.0,1.0,10000)
  f1.plotItself
  Helper.integrate(math.Pi/2.0)
  //println(Helper.integrate(1.295874008731708087790412491527))

}
