
import scala.collection.mutable.ListBuffer
import pe037.primesUnder

object pe041 {
  println("In object")
  
  def factorial(x: BigInt): BigInt = if(x == 0){1}else{x * factorial(x - 1)}
  
  def all_7pandigital(): List[List[Int]] = {
    var this_index = 0
    var select_position = 0
    val pan_list = new ListBuffer[List[Int]]
    var num_list = new ListBuffer[Int]
    var this_pan_list = new ListBuffer[Int]
    val div_list = new ListBuffer[Int]
    
    for (i <- 6 to 1 by -1){
      div_list += factorial(i).toInt
    }
    
    //for (i <- 0 to (factorial(9).toInt-1)) {
    for (i <- 0 to (factorial(7).toInt-1)) {
      num_list = ListBuffer(7,6,5,4,3,2,1)
      this_pan_list = ListBuffer.empty
      this_index = i
      for (j <- 0 to 5){
        select_position = math.floor(this_index/div_list(j)).toInt
        //println(i + "," + j + "," + this_index + "," + select_position + "," + num_list(select_position))
        this_index -= select_position * div_list(j)
        this_pan_list += num_list(select_position)
        //println(this_pan_list)
        num_list -= num_list(select_position)
      }
      this_pan_list += num_list(0) 
      //println(this_pan_list)
      pan_list += this_pan_list.toList
    }
    pan_list.toList
  }
  
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
  
  def list_9dig_to_num(n_list:List[Int]):Int = {
    var sum =0
    for(i<-0 to 8){
      sum += n_list(i) * math.pow(10,8-i).toInt
    }
    sum
  }
  
  def list_7dig_to_num(n_list:List[Int]):Int = {
    var sum =0
    for(i<-0 to 6){
      sum += n_list(i) * math.pow(10,6-i).toInt
    }
    sum
  }
  def main (args: Array[String]): Unit = {
    println("In main")
    val pan_list = all_7pandigital()
    println("got pandigital list")
    var prime_list = primesUnder(7654321+1)
    println("got primes list")
    
    for(a <- 0 to pan_list.length.toInt-1){
      if(prime_list.find( _ == list_7dig_to_num(pan_list(a)) ).isDefined)
      {println(pan_list(a))}
    }
    
    //println(prime_list)
  }
}