package Integral

import breeze.linalg._
import breeze.plot._
import breeze.optimize._

object IntegralSolver extends App{
  object Helper{

    def integrate(rightAnswer: Double) = {
      lazy val lz: LazyList[Int] =  4 #:: lz.map(f => f * 2)
      val y: List[Double] = lz.takeWhile(e => e < math.pow(2,15)).toList.map(f => {
        //val func = inv1plusSquared(-1,1,f)
        val func = expSin(-1,1,f)
        val method = Trapezoidal(func)
        //val method = Simpson(func1)
        val s = rightAnswer - method.calculate()
        s
      })
      println(y)
      val x = linspace(0.0, 3 * math.Pi,y.length)
      val fig = Figure()
      val p = fig.subplot(0)
      p += plot(x,y)
      fig.refresh
    }
  }

  val f1 = expSin(-1.0,1.0,10000)
  f1.plotItself
  Helper.integrate(math.Pi/2.0)
  //println(Helper.integrateTrap(math.Pi/2))

}
