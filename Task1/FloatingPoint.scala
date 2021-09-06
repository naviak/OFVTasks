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
      val ls = s.takeWhile(_ != ev.minus(one,one)).toList
      ls.length - 1
    }

    def loopNumMantiss(): Int = {
      var zeroOne: F = ev.div(one,base)
      var num: F = one + zeroOne
      var counter: Int = 1
       while(num != one){
         zeroOne = ev.div(zeroOne,base)
         num = one + zeroOne
         counter += 1
      }
      counter
    }
    def lazyNumMantiss(): Int = {
      var zeroOne = ev.div(one,base)
      lazy val s: LazyList[F] = (one + zeroOne) #:: s.map(f =>{
        zeroOne = ev.div(zeroOne,base)
        one + zeroOne
      })
      val ls = s.takeWhile(_ != one).toList
      ls.length + 1
    }
  }

  val floatNumber = new FloatNumber(1.0f,10.0f)
  val doubleNumber = new FloatNumber(1.0,10.0)

  println(s"epsilon for Float:\t ${floatNumber.epsilon}")
  println(s"min exponent of Float:\t ${floatNumber.minExponent}")
  println(s"max exponent of Float:\t ${floatNumber.maxExponent}")
  println(s"mantissa of Float calculated by using loop:\t ${floatNumber.loopNumMantiss}")
  println(s"mantissa of Float calculated by using lazy:\t ${floatNumber.lazyNumMantiss}")

  println("")

  println(s"epsilon for Double:\t ${doubleNumber.epsilon}")
  println(s"min exponent of Double:\t ${doubleNumber.minExponent}")
  println(s"max exponent of Double:\t ${doubleNumber.maxExponent}")
  println(s"mantissa of Double calculated by using loop:\t ${doubleNumber.loopNumMantiss}")
  println(s"mantissa of Double calculated by using lazy:\t ${doubleNumber.lazyNumMantiss}")
}
