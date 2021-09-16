
import breeze.linalg._
import breeze.plot._
import breeze.optimize._
object QuantEquation {
  object ChrestomaticEquation {
    val a = 10
    private val U0 = 1000
    private val fig = Figure()
    val start = -3
    val end = -5
    val p = fig.subplot(0)

    def f(x: Double) = {
      val ctgSqrtArg = 2 * a * a * U0 * (1 + x / U0)
      val ctgArg = math.sqrt(ctgSqrtArg)
      val ctg = 1 / math.tan(ctgArg)
      val rightArg = -U0 / x - 1
      val right = math.sqrt(rightArg)
      ctg - right
    }

    def plotF[T <: Method](method: T): Unit = {
      val x = linspace(-1000, 0, 1000000)
      val y = x.map {
        case x => val t = f(x)
          if (t.isNaN) 0
          else if (t.isInfinite) 1e4
          else t
      }
      p += plot(x, y)

      val sol = method.solve(-4.7, f,1e-10)
      p += plot(DenseVector.fill(1000)(sol), linspace(-120000, 120000, 1000), '.')
      println(f(sol))
      p.xlabel = "x axis"
      p.ylabel = "y axis"
      fig.refresh()
    }
  }
  def main(args:Array[String]): Unit = {
    val newton = new Newton()
    val iter = new SimpleIterations()
    val dich = new Dichotomy()
      ChrestomaticEquation.plotF(iter)
  }

}