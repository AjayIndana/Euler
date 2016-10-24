/**
  * Created by aindana on 10/21/2016.
  */
class Problems extends BaseTest{

  def Problem1() {
    var sum: Int = 0
    for(i <- 1 until 1000)
    {
      val temp: Int = if(i%3 == 0) i else if(i%5 == 0) i else 0
      sum = sum + temp
    }
    println(sum)
  }

  def Problem2() {
   var num1 = 1
   var num2 = 2
   var sum = 0
    var evenSum = 2
    while(sum < 4000000) {
      sum = num1 + num2
      num1 = num2
      num2 = sum
      val temp = if(sum%2 == 0) sum else 0
      evenSum = evenSum + temp
    }
    println(evenSum)
  }

  def Problem3() {

    println(largestPrimeFactor(600851475143L))
  }

  def Problem4() {

   //listOfPrimes(1000000)

      println("Final : "+ consecutivePrime(1000000, 540))
  }
}
