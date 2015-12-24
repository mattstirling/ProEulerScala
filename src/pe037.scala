
import scala.collection.mutable.ListBuffer

object pe037 {
  println("In object")
  
  def primesUnder(n: Int): List[Int] = {
    require(n >= 2)
    val primes = ListBuffer(2)
    for (i <- 3 to n) {
      if (prime(i, primes.iterator)) {
        primes += i
      }
    }
  primes.toList
  }

  def prime(num: Int, factors: Iterator[Int]): Boolean = factors.takeWhile(_ <= math.sqrt(num).toInt) forall(num % _ != 0)
  
  def prime_left_truncatable(num: Int, primes: List[Int]): Boolean = {
    val num_digits = math.floor(math.log(num)/math.log(10)).toInt
    val next_num = (num%(math.pow(10,num_digits))).toInt
    if (primes.takeWhile(_ <= next_num) forall(_ != next_num)) {false}
    else{
      if (num_digits == 1){true}
      else {prime_left_truncatable(next_num,primes)
      }
    }
  }
  
  def prime_right_truncatable(num: Int, primes: List[Int]): Boolean = {
    val next_num = ((num - num%10)/10).toInt
    if (primes.takeWhile(_ <= next_num) forall(_ != next_num)) {false}
    else{
      val num_digits = math.floor(math.log(num)/math.log(10)).toInt
      if (num_digits == 1){true}
      else {prime_right_truncatable(next_num,primes)
      }
    }
  }
  
  def main (args: Array[String]): Unit = {
    println("Scala rocks!")
    var sum = 0L
    val prime_list = primesUnder(10000000)
    for(a <- prime_list){
      if (prime_left_truncatable(a,prime_list.toList) && prime_right_truncatable(a,prime_list.toList)){
        println(a)
        sum += a 
      }
    }
    
    println("sum is: " + sum)
    //println(prime_list)
  }
}