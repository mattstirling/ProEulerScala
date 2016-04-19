import scala.collection.mutable.ArrayBuffer
import PE_Lib.primesUnder
import PE_Lib.binarySearch

object pe046 {
  
  def is_composite_and_not_prime_plus_square(num:Long,prime_array:Array[Long]):Boolean = {
    var i = 1
    var this_num = num
    while(this_num > 0){
      if(PE_Lib.binarySearch(prime_array,this_num) > 0){return false}
      this_num = (num - 2*i*i).toLong
      i+=1
    }
    true  
  }
  
  def main (args: Array[String]): Unit = {
    val prime_a = PE_Lib.primesUnder(10000000)
    
    
    for(i<- 3 to 1000001 by 2){
      //println(i + "," + is_composite_and_not_prime_plus_square(i.toLong,prime_a))
      if (is_composite_and_not_prime_plus_square(i.toLong,prime_a)){
        println(i + "," + is_composite_and_not_prime_plus_square(i.toLong,prime_a))
      }
    }
    
    
    for(i<-prime_a.takeWhile(_ < 6000)){
      println(i)
    }
    
  }
}