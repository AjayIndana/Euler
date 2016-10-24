package AllLevels

/**
  * Created by aindana on 10/21/2016.
  */

import scala.annotation.tailrec
class BaseTest {



  def sum(xs: List[Int]): Long = {
    @tailrec
    def inner(xs: List[Int], accum: Long): Long = {
      xs match {
        case x :: tail => inner(tail, accum + x)
        case Nil => accum
      }
    }
    inner(xs, 0)
  }


  def product(xs: List[Int]): Long = {
    @tailrec
    def inner(xs: List[Int], accum: Long): Long = {
      xs match {
        case x :: tail => inner(tail, accum * x)
        case Nil => accum
      }
    }
    inner(xs, 1)
  }

  def factorial(number: Int) : BigInt = {
    @tailrec
    def factorialWithAccumulator(accumulator: BigInt, number: Int) : BigInt = {
      number match {
        case 1 => accumulator
        case _ => factorialWithAccumulator(accumulator * number, number - 1)
      }

    }
    factorialWithAccumulator(1, number)
  }


}
