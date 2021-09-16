import breeze.linalg._

import scala.annotation.tailrec
import QuantEquation.ChrestomaticEquation

trait Method {
  def solve(x0: Double, f: (Double) => Double, eps: Double): Double
}



