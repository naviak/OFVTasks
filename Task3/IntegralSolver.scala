package Integral

import breeze.linalg._
import breeze.plot._
import breeze.optimize._

object IntegralSolver extends App{
  val ls = linspace(0.0, math.Pi,1000).toArray.toList
  val trap = Trapezoidal(math.sin)
  println(trap.calculate(ls))

  val simp = Simpson(math.sin)
  println(simp.calculate(ls))

}
