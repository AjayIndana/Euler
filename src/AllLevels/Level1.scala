package AllLevels


import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.math._
import scala.util.control.Breaks._

/**
  * Created by aindana on 10/21/2016.
  */
object Level1 extends BaseTest{


  def amicablePair(): Unit =
  {
    var num1 = 2
    var allList = ArrayBuffer[Int]()
    while(num1<10000)
    {
      var num2 = totalFactors(num1).toInt
      if(totalFactors(num2)==num1 && num1!=num2)
      {
        allList += num1

      }

      num1+=1
    }
    println(allList)
    println(sum(allList.toList))
  }

  def totalFactors(value:Int): Long ={
    var factor = 1
    var allList = ArrayBuffer[Int]()
    allList += 1
    var allFactorList = ArrayBuffer[Int]()
    var PrimeList1 = listOfPrimes(value)
    for(i<-PrimeList1){

      var primeFactNum = primeFactorNumber(value,i,0)
      for(j<-1 to primeFactNum)
      {
        for(k<-allList)
        {
          var num = k*i
          if(!allList.contains(k*i))
          {
            allList += k*i
          }
        }

      }
      factor = factor * (primeFactNum + 1)

    }

    sum(allList.toList.dropRight(1))
  }

  def primeFactorNumber(xs: Int, div:Int, count:Int): Int ={
    xs%div match {
      case 0 => primeFactorNumber(xs/div, div, count+1)
      case _ => count
    }
  }

  def highDivisible(): Unit ={
    var factor =1
    var factor2 =1
    var num = 2

    // println(num)
    while(factor<=500)
    {
      var PrimeList1 = List(0)
      var PrimeList2 = List(0)
      factor = 1
      factor2 =1
      var num1 = 0
      var num2 = 0
      if(num%2 == 0) {
        num1 = num/2
        num2 = num+1
        PrimeList1 = listOfPrimes(num/2.toInt)
        PrimeList2 = listOfPrimes((num+1).toInt)
      }
      else
      {
        num1 = num
        num2 = (num+1)/2
        PrimeList1 = listOfPrimes(num.toInt)
        PrimeList2 = listOfPrimes((num+1)/2.toInt)
      }
      for(i<-PrimeList1){
        factor = factor * (primeFactorNumber(num1.toInt,i,0) + 1)

      }
      // println("factor1 " + factor)
      for(i<-PrimeList2){
        factor2 = factor2 * (primeFactorNumber(num2.toInt,i,0) + 1)

      }
      // println("factor2 " + factor2)
      factor = factor * factor2
      println("Number1 :" + num1 + "Number2 :" + num2 + " factor : " + factor + " Sum : " + ((num+1)*num)/2 )
      num+=1
    }
    println(num-1)
    println(((num-1)*num)/2 + " " + factor)
  }

  def evenDivisable: Unit =
  {
    var flag = 0
    var num = 40
    while(flag<20) {
      breakable {
        for (i <- 1 to 20) {
          if (num % i == 0) {
            flag += 1
          }
          else {
            break
          }
        }
      }
      if(flag < 20)
      {
        flag = 0
        num+=20
      }
    }
    println(num)
  }

  def sumSquareDiff: Unit =
  {
    var num :Double= 100
    var sum = (num*(num+1))/2
    var sumSquare = sum*sum
    println(sumSquare.toLong)
    var squareSum = scala.math.pow(num,3)/3 + scala.math.pow(num,2)/2 + num/6
    println(squareSum.toLong)
    val diff = sumSquare - squareSum
    println(diff.toLong)

  }

  def palindrome(): Unit =
  {
    var max = 0
    for(i <- 100 until 1000)
    {
      for(j <- 100 until 1000){
        if(isPalidrome(i*j))
        {
          if(i*j > max)
          {
            max = i*j
          }
        }
      }
    }
    println(max)
  }

  def isPalidrome(value: Int) : Boolean =
  {

    //  println(newString)
    var rev = value.toString.reverse.toInt
    //  println(rev)
    if(rev==value) true else false
  }

  def isPrime(value: Long): Boolean =
  {
    var flag = 0
    var i = 2
    while(i<value && flag==0)
    {
      var temp = if(value%i == 0) 1 else 0
      flag = flag + temp
      i=i+1
    }

    if(flag == 0) true else false
  }


  def largestPrimeFactor(value: Long): Long =
  {
    var largestPrimeFactor: Long = 0
    var i = value/2
    var j = 2
    while(i>1 && largestPrimeFactor==0)
    {
      i=value/j
      //  println("Checking: " + i)
      if(value%i == 0)
      {
        if(isPrime(i))
        {
          largestPrimeFactor = i

        }
      }
      j=j+1
    }
    largestPrimeFactor
  }


  def listOfPrimes(value: Int): List[Int] =
  {
    //Sieve_of_Eratosthenes
    var primeList = ArrayBuffer[Int]()

    var isPrime = Array.tabulate(value+1)(n => true)

    var factor = 2
    while(factor*factor <= value)
    {
      if(isPrime(factor))
      {
        var j = factor
        while(factor*j<=value)
        {
          isPrime(factor*j) = false
          j+=1
        }
      }
      factor+=1
    }

    var primes = 0
    var i=2
    while(i<=value)
    {
      if(isPrime(i))
      {
        primeList+=i
      }
      i+=1
    }
    //  println(primeList.length)
    primeList.toList

  }

  def consecutivePrime(value: Int, setLength :Int) : Int = {
    val primeList = listOfPrimes(value)
    val totalLenght = primeList.length
    var sum=0
    var originSum = 0
    var length = setLength
    var foundLength =0
    var ini =0
    breakable{
      while(ini < (totalLenght - length))
      {
        sum = getSum(ini,ini+length,value,primeList)
        if(primeList.contains(sum))
        {
          originSum = sum
          length+=1
          ini = 0

          foundLength = (length-1)
          println("Found " + (length-1) + " " + originSum )
        }
        else ini+=1
        if(ini >= (totalLenght - length))
        {
          length+=1
          ini=0
        }
      }
    }
    foundLength
  }


  def getSum(index1:Int, index2:Int, limit: Int, primeList :List[Int]): Int =
  {
    var sum = 0

    var newList = primeList.slice(index1, index2).reverse
    var i=0
    breakable {
      while (i < newList.length) {
        sum = sum + newList(i)
        if (sum > limit) {
          sum = 0
          break
        }
        i += 1
      }
    }

    sum
  }

  def getFinalValue(value: Int): Int =
  {
    var minlength = 2
    var length =2
    var newValue = 100

    if(value>100)
    {
      minlength = getFinalValue(100)
      val repeat = log10(value).toInt - 2
      for(i <- 1 to repeat)
      {
        newValue = newValue * i * 10
        minlength = consecutivePrime(newValue,minlength)
      }
    }

    if(value == 100) {

      length = consecutivePrime(value,2)

    }

    else
    {
      length = getFinalValue(value/10)
    }

    length
  }


  def findPrime(value: Int) : Int =
  {

    //Sieve_of_Eratosthenes
    var primeList = ArrayBuffer[Int]()

    var isPrime = ArrayBuffer.tabulate(value+2)(n => true)

    var newIsPrime = isPrime
    var factor = 2
    var counter = 0

    while (factor * factor <= isPrime.length-1) {
      if (isPrime(factor)) {
        var j = factor
        breakable {
          while (factor * j <= isPrime.length-1) {
            if (isPrime(factor * j)) {
              // isPrime += true
              isPrime(factor * j) = false
              isPrime += true
              j=2
              factor=2
            }
            else
              j += 1


          }
        }
      }

      factor += 1
    }

    // println(isPrime.toList)
    var primes = 0
    var i=2
    while(i<=isPrime.length-1)
    {
      if(isPrime(i))
      {
        primeList+=i
      }
      i+=1
    }
    // println(primeList.toList.last)
    primeList.toList.last
  }


  def largestProduct(): Unit =
  {
    val bigNumber = "73167176531330624919225119674426574742355349194934" +
      "96983520312774506326239578318016984801869478851843" +
      "85861560789112949495459501737958331952853208805511" +
      "12540698747158523863050715693290963295227443043557" +
      "66896648950445244523161731856403098711121722383113" +
      "62229893423380308135336276614282806444486645238749" +
      "30358907296290491560440772390713810515859307960866" +
      "70172427121883998797908792274921901699720888093776" +
      "65727333001053367881220235421809751254540594752243" +
      "52584907711670556013604839586446706324415722155397" +
      "53697817977846174064955149290862569321978468622482" +
      "83972241375657056057490261407972968652414535100474" +
      "82166370484403199890008895243450658541227588666881" +
      "16427171479924442928230863465674813919123162824586" +
      "17866458359124566529476545682848912883142607690042" +
      "24219022671055626321111109370544217506941658960408" +
      "07198403850962455444362981230987879927244284909188" +
      "84580156166097919133875499200524063689912560717606" +
      "05886116467109405077541002256983155200055935729725" +
      "71636269561882670428252483600823257530420752963450"

    val charList = bigNumber.toList

    val length = 13
    var  i=0
    var maxProduct: Long =1
    while(i<charList.length-length)
    // while(i<5)
    {
      var newList = charList.slice(i,i+length)
      //  println(newList)
      var Product: Long =1
      for(c<-newList)
      {
        Product = Product*c.toString.toInt
      }
      // println(Product)
      if(Product > maxProduct) maxProduct=Product
      i+=1
    }
    println(maxProduct)
  }

  def Pythogran(): Unit =
  {
    var i=1;
    var squares = ArrayBuffer[Int]()
    while(i<1000)
    {
      squares += i*i
      i+=1
    }
    var a=1
    var b=2
    var counter =0
    while(a<1000)
    {
      while(b<1000)
      {
        val sum = a*a + b*b
        if(squares.toList.contains(sum) && (sqrt(sum)+a+b)==1000)
        {
          println(sqrt(sum).toInt*a*b)

        }
        b+=1
      }
      a+=1
      b=a+1
    }
  }

  def sumOfPrimes(): Unit =
  {
    //println(listOfPrimes(200))
    var listPrimes = listOfPrimes(2000000)

    println(sum(listPrimes))
  }



  def Collatz(value: Long): Unit =
  {
    var maxKey: Long =0
    var maxVal: Long=0
    var collatz = scala.collection.mutable.Map[Long, Long](1L -> 1L)
    var i: Long= 0
    while(i<value)
    {
      var value = chain(i, 0, collatz)
      collatz += (i -> value)
      if(value>maxVal)
      {
        maxVal = value
        maxKey = i
      }
      i+=1
    }

    println(maxKey)
    println(maxVal)
  }


  def chain(num: Long, chainLength: Long, collatz: scala.collection.mutable.Map[Long, Long]): Long ={

    if(num>1) {
      num % 2 match {
        case 1 => {
          if(collatz.contains(num))
          {
            chainLength + collatz(key = num)
          }
          else {
            chain(3 * num + 1, chainLength + 1, collatz)
          }
        }
        case _ => {
          if(collatz.contains(num))
          {
            chainLength + collatz(key = num)
          }
          else {
            chain(num / 2, chainLength + 1, collatz)
          }
        }
      }
    }
    else {
      (chainLength+1)
    }

  }

  def fact(value: Int): BigInt =
  {
    value match {
      case 0 => 0
      case 1 => 1
      case _ => value*fact(value-1)
    }

  }

  def power(value: Int): Long =
  {
    var ini: BigInt = 1
    var j =0
    while(j<value)
    {
      ini = 2*ini
      j+=1
    }
    val bigList = ini.toString().toList
    // println(bigList)
    var i = 0
    var sum: Long = 0
    while(i<bigList.length)
    {
      sum += bigList(i).toString.toInt
      i+=1
    }
    sum

  }

  def powerTwo(value : Int): Unit =
  {
    var power = ArrayBuffer[Int]()

    power += 1

    var i = 0;
    while(i<value)
    {
      var j =0
      while(j<power.length)
      {
        var prod = power(j) * 2
        power(j) = prod % 10
        var carry = prod/10

        j+=1
      }
    }
  }

  def digitToString(value: Int): String =
  {
    var suffixOnes = ""
    var suffixTens = ""
    var suffixHundred = ""
    var tens=0
    var hundreds=0
    var ones = value%10

    ones match {
      case 1 => suffixOnes ="one"
      case 2 => suffixOnes ="two"
      case 3 => suffixOnes= "three"
      case 4 => suffixOnes ="four"
      case 5 => suffixOnes ="five"
      case 6 => suffixOnes ="six"
      case 7 => suffixOnes ="seven"
      case 8 => suffixOnes ="eight"
      case 9 => suffixOnes ="nine"
      case _ => suffixOnes =""
    }

    if(value>=10)
    {
      tens = (value%100)/10

      tens match {
        case 0 =>{
          suffixTens = ""
        }
        case 1 => {

          ones match {
            case 1 => suffixOnes ="eleven"
            case 2 => suffixOnes ="twelve"
            case 3 => suffixOnes= "thirteen"
            case 4 => suffixOnes ="fourteen"
            case 5 => suffixOnes ="fifteen"
            case 6 => suffixOnes ="sixteen"
            case 7 => suffixOnes ="seventeen"
            case 8 => suffixOnes ="eighteen"
            case 9 => suffixOnes ="nineteen"
            case 0 => {suffixOnes="";suffixTens = "ten"}
          }
        }
        case 2 => suffixTens ="twenty "
        case 3 => suffixTens= "thirty "
        case 4 => suffixTens ="forty "
        case 5 => suffixTens ="fifty "
        case 6 => suffixTens ="sixty "
        case 7 => suffixTens ="seventy "
        case 8 => suffixTens ="eighty "
        case 9 => suffixTens ="ninety "
      }
    }

    if(value>=100) {
      suffixHundred = " hundred"
      if(value%100>0)
      {
        suffixHundred = suffixHundred + " and"
      }
      hundreds = (value%1000)/100

      hundreds match {
        case 1 => suffixHundred = "one"+ suffixHundred
        case 2 => suffixHundred ="two"+ suffixHundred
        case 3 => suffixHundred ="three"+ suffixHundred
        case 4 => suffixHundred ="four"+ suffixHundred
        case 5 => suffixHundred ="five"+ suffixHundred
        case 6 => suffixHundred ="six"+ suffixHundred
        case 7 => suffixHundred ="seven"+ suffixHundred
        case 8 => suffixHundred ="eight"+ suffixHundred
        case 9 => suffixHundred ="nine"+ suffixHundred
      }
    }

    suffixHundred + " " + suffixTens + suffixOnes


    /*case 3 => suffixHundred = "hundred and "
    case 4 => {suffixThousand = "thousand and " ; suffixHundred = "hundred and "}*/
  }

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
