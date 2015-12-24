

object pe036 {
  println("In object")
  
  def is_palindrome(str_num: String): Boolean = {
    var result = str_num.reverse.mkString == str_num
    result
  }
  
  def num_to_str_bin(n:Long): String = {
    var this_num = n
    var str_num = ""
    do{str_num = str_num + this_num%2
      this_num = (this_num - this_num%2)/2
    }while(this_num > 1)
    str_num = "1" + str_num.reverse.mkString
    str_num
  }
  
  def is_bin_palindrome(n: Long): Boolean = {
    var str_num = num_to_str_bin(n)
    var result = is_palindrome(str_num)
    result
  }
  
  
  def main (args: Array[String]): Unit = {
    println("Scala rocks!")
    var sum = 0
    for( a <- 1 to 1000000){
      if(is_palindrome(a.toString())){
        if(is_bin_palindrome(a)){
          sum += a 
        }
      }
    }
    println( sum)  
  }
}