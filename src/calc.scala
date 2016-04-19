

object calc {
  
  def factorial(x: BigInt): BigInt = if(x == 0){1}else{x * factorial(x - 1)}
  
  def main (args: Array[String]): Unit = {
    println("hi")
    println(factorial(9).toInt)
    println(9*factorial(9).toInt)
    println(9*factorial(9).toInt-1)
  }
}