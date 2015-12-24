import scala.io.Source

import scala.collection.mutable.ListBuffer
import pe037.primesUnder

object pe042 {
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
  
  def main (args: Array[String]): Unit = {
    println("In main")
    
    val filename = "C:/Temp/scala/in/p042_words.txt"
    for (line <- Source.fromFile(filename).getLines) {
      println(line)
    }
  }
}