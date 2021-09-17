
import breeze.linalg._
import breeze.plot._
import breeze.optimize._

object QuantEquation {
  object ChrestomaticEquation {
    val a = 10
    private val U0 = 100
    private val fig = Figure()
    val p = fig.subplot(0)

    val razr = endOfGraph
    val start = razr
    val end = U0

    private def endOfGraph(): Double = {
      val razr = (1 - Math.pow(Math.PI, 2) / (2 * a * a * U0))*U0
      if(razr < 0) 0
      else razr
    }

    def ctgArg(x: Double): Double = {
      val ctgSqrtArg = 2 * a * a * U0 * (1 - x / U0)
      math.sqrt(ctgSqrtArg)
    }

    def f(x: Double) = {
      val ctg = 1 / math.tan(ctgArg(x))
      val rightArg = U0 / x - 1
      val right = math.sqrt(rightArg)
      ctg - right
    }

    def plotF[T <: Method](method: T): Unit = {
      println(endOfGraph())
      val x = linspace(0, U0, 10000)
      val y = x.map {
        case x => val t = f(x)
          if (t.isNaN) 0
          else if (t.isInfinite || t > 1000 ) 1e2
          else t
      }
      p += plot(x, y)
      val sol = method.solve((start + end) / 2, f, 1e-10)
      p += plot(DenseVector.fill(1000)(sol), linspace(-120, 120, 1000), '.')
      println(f(sol))
      p.xlabel = "x axis"
      p.ylabel = "y axis"
      fig.refresh()
    }
  }

  def main(args: Array[String]): Unit = {
    val newton = new Newton()
    val iter = new SimpleIterations()
    val dich = new Dichotomy()
    ChrestomaticEquation.plotF(newton)
  }

}