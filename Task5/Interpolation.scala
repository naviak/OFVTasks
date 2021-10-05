package Interpolation

import breeze.linalg.{DenseVector, linspace}
import breeze.plot.{Figure, plot}

object Interpolation extends App {

  case class Formula(n: Int) {
    val x: DenseVector[Double] = DenseVector((0.0 :: List.tabulate(n)(_ + 1.0)).map(k => xk(k)).toArray)
    val y: DenseVector[Double] = DenseVector((0.0 :: List.tabulate(n)(_ + 1.0)).map(k => yk(k)).toArray)

    def xk(k: Double): Double = {
      1 + k / n
    }

    def yk(k: Double): Double = {
      math.log(xk(k))
    }
  }

  final case class InterpolatorFormulae(formula: Formula) {
    val x: DenseVector[Double] = linspace(1.0, 2.0, 1000)
    val y: DenseVector[Double] = x.map(t => pn(t))

    def li(iX: Double, i: Double): Double = {
      (0.0 :: List.tabulate(formula.n)(_ + 1.0)).fold(1.0) { (acc, j) => if (i != j) acc * (iX - formula.xk(j)) else acc }
    }

    def pn(iX: Double): Double = {
      (0.0 :: List.tabulate(formula.n)(_ + 1.0)).fold(0.0) { (acc, j) => acc + formula.yk(j) * li(iX, j) / li(formula.xk(j), j) }
    }
    def getInterpolationError(): DenseVector[Double] = {
      val temp = Formula(999)
      y - temp.y
    }
  }

  val f = Figure()
  val p = f.subplot(0)

  val f1 = Figure()
  val p1 = f1.subplot(0)

  val f2 = Figure()
  val p2 = f2.subplot(0)

  Range(4, 15).foreach(t => {
    val formula = Formula(t)
    val interpolated = InterpolatorFormulae(formula)
    p += plot(formula.x, formula.y, name = s"n = $t")
    //p += plot(interpolated.x, interpolated.y, name = s"ns = $t")
    p1 += plot(interpolated.x, interpolated.y, name = s"n = $t")
    p2 += plot(interpolated.x, interpolated.getInterpolationError().map(t => math.log10(math.abs(t) + 1e-15) / math.log(2)), name = s"n = $t")
  })
  p.setYAxisDecimalTickUnits()
  p.title = "discrete"
  p.legend = true
  p.setXAxisDecimalTickUnits()

  p1.setXAxisDecimalTickUnits()
  p1.setYAxisDecimalTickUnits()
  p1.title = "interpolated"
  p1.legend = true

  p2.setXAxisDecimalTickUnits()
  p2.setYAxisIntegerTickUnits()
  p2.title = "error"
  p2.legend = true

  f.refresh()
  f1.refresh()
  f2.refresh()

}
