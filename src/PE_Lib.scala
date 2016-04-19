import scala.collection.mutable.ArrayBuffer

object PE_Lib {
  
  def primesUnder(n: Long): Array[Long] = {
    require(n >= 1)
    if(n==1){return Array.emptyLongArray}
    val primes = ArrayBuffer(2L)
    for (i <- 3 to n.toInt) {
      if (prime(i, primes.iterator)) {
        primes += i.toLong
      }
    }
  primes.toArray
  }

  def prime(num: Long, factors: Iterator[Long]): Boolean = factors.takeWhile(_ <= math.sqrt(num).toInt) forall(num % _ != 0)
  
  def binarySearch(a:Array[Long],key:Long) = {
    java.util.Arrays.binarySearch(a.asInstanceOf[Array[Long]],key)
  }
  
  def gcd(a:Long,b:Long):Long = {
    // use Euclid's algorithm
    if(b==0){return a}
    else(return gcd(b, a % b))
  }
  
  def is_coprime(a:Long,b:Long):Boolean = {
    gcd(a,b)==1
  }
  
  
  def main (args: Array[String]): Unit = {
    println(gcd(21L,9L))
  }
 
  
  
}