package Task1

import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.Ordered.orderingToOrdered
//import org.apache.commons.math.util.MathUtils

object FloatingPoint extends App{
  class FloatNumber[F](oneInSystem: F,base: F)(implicit ev: Fractional[F]) {
    private val one:F = oneInSystem
    def epsilon(): F = {
      lazy val s: LazyList[F] = one #:: s.map(f => ev.div(f, ev.plus(one, one)))
      s.takeWhile(e => ev.plus(e, one) != one).last
    }

    def maxExponent(): Int = {
      lazy val s: LazyList[F] = one #:: s.map(f => ev.times(f, base))
      val ls = s.takeWhile(_ < ev.div(one,ev.minus(one,one))).toList
      ls.length - 1
    }

    def minExponent(): Int = {
      lazy val s: LazyList[F] = one #:: s.map(f => ev.div(f, base))
      val ls = s.takeWhile(_ != 0f).toList
      ls.length - 1
    }
  }

  val floatNumber = new FloatNumber(1.0f,10.0f)
  val doubleNumber = new FloatNumber(1.0,10.0)

  val s: LazyList[Float] = 1f #:: s.map(f => f / 10f)
  val ls = s.takeWhile(_ < 1f).toList

  println(floatNumber.epsilon)
  println(floatNumber.minExponent)
  println(floatNumber.maxExponent)

  println(doubleNumber.epsilon)
  println(doubleNumber.minExponent)
  println(doubleNumber.maxExponent)
  println(1f/0f)
}
