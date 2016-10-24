/**
  * Created by aindana on 10/21/2016.
  */

import scala.collection.mutable.ArrayBuffer
import scala.math._
object Euler extends Problems{
  def main(args: Array[String]): Unit = {
    var max = 0
    var maxa =0
    var maxb =0
    var a = -999
    while(a<1000) {
      var b = -1000
      while(b<=1000) {
        var n = 0
        var eval = n * n + a * n + b
        if (eval < 0) eval = eval * (-1)
        while (totalFactors(eval) == 1) {
          n += 1
          eval = n * n + a * n + b
          if (eval < 0) eval = eval * (-1)

        }

        if(n>max)
          {
            max=n
            maxa=a
            maxb=b
          }

     //   println("a "+ a + " b " +b)

        b+=1
      }
      a+=1
    }

    println(max)
    println(maxa)
    println(maxb)
    println(maxa*maxb)

  }

  def getFraction(divisor: Int): Int = {
    val num =1

    var k = divisor
    var numer = num
    var divi = 0
    var fraction : String = "0."
    var decimal = ArrayBuffer[Int]()
    var numerator = ArrayBuffer[Int]()
    numerator += num
    var result = 0
    var recur = 0
    while(numer>0)
      {

        numer = numer*10
        if(!numerator.contains(numer)) {
        numerator+=numer
        divi = numer/k

          recur =0
          decimal+=divi
          fraction += divi.toString
          numer = numer % k
        }
        else {
          result = numerator.length - numerator.indexOf(numer)
          numer = 0
          recur = 1
        }
      }
  //  println(numerator)
  //  println(fraction)
    result
  }

  def fib(): Unit = {
    var a: BigInt =1
    var b: BigInt =1
    var c:BigInt = 0
    var term: Int =2
    while(c.toString().length < 1000)
      {
        c=a+b
        a=b
        b=c
        term+=1
      //  println(c)
      }
    println(term)
  }

  def lexi(): Unit ={

    var maxvalue = 999999

  //  while(maxvalue<24) {
      var combi = ArrayBuffer.range(0,10)
      var value: BigInt = maxvalue
      while (value > 0) {

        var near = nearestFact(value)
    //    println("near " + near)
        var a = combi.length - near -1
    //     println("a "+a)
        var b=a
        while(a>=b) {

          b = combi.indexOf(combi(b) + 1)
    //      println("b " + b)
        }
        combi = swap(combi, a, b)
    //    println(combi)
        value = value - fact(near)


      }
      println(combi)
  //    println("----------")
      maxvalue+=1
   // }
  }

  def swap(combi: ArrayBuffer[Int], a: Int, b: Int): ArrayBuffer[Int] ={
    combi(a) = combi(a) + combi(b)
    combi(b) = combi(a) - combi(b)
    combi(a) = combi(a) - combi(b)
    combi
  }

  def nearestFact(value: BigInt): Int = {
    var num = 1
    var factor: BigInt=0
    while(factor<=value)
      {
        num+=1
        factor = fact(num)
      }
   //if(factor==value) num-=1
    //println(num-1)
    num-1
  }

  def abundantSums(): List[Int] ={
    var i = 0
    var abd = ArrayBuffer[Int]()
    var abdSums = ArrayBuffer[Int]()
    while(i<=30) {
      if(totalFactors(i)>i && totalFactors(i)<=30)
        {
          abd+=i
      //    println(i)
          var j=0
          while(j<abd.length)
            {
              var value = abd(j) + i

              if(value<=30) {
                if (!abdSums.contains(value)) {
              //    println(value)
                  abdSums += value
                }
              }
              j+=1
            }
        }
      i+=1
    }
    println(abdSums.toList)
    abdSums.toList
  }

  def countSundays(): Int =
  {
    var year = 1900
    var day = 1
    var count = 0
    while(year<=2000)
      {
        var months = 1
        while(months<=12){
         if(months==1) { if(isSunday(day)) count+=1; day += 31}
         else if(months==2 && isLeapYear(year)) { if(isSunday(day)) count+=1; day += 29 }
         else if(months==2 && !isLeapYear(year)) { if(isSunday(day)) count+=1;day += 28}
         else if(months<=7 && months%2==1) { if(isSunday(day)) count+=1;day += 31}
         else if(months<=7 && months%2==0) { if(isSunday(day)) count+=1;day += 30}
         else if(months>7 && months%2==0) { if(isSunday(day)) count+=1;day += 31}
         else if(months>7 && months%2==1) { if(isSunday(day)) count+=1;day += 30}

          months+=1
        }

        year+=1
      }
    count
  }

  def isSunday(day: Int): Boolean =
  {
    if(day%7 ==0) true else false
  }

  def isLeapYear(year: Int): Boolean =
  {
    if(year%400 !=0 && year%4 == 0) true else false
  }






  def nameScore(nameList : List[String]): Unit =
  {
    var num = 1
    var sum: Long =0
    for(i<-nameList)
      {
        var sum2 = 0
        for(c<-i.toList)
          {
            sum2 += (c.toInt-64)
          }
        sum2 = sum2*num
        sum = sum + sum2
        num+=1
      }
    println(sum)
  }

}
