
import scala.collection.mutable.ListBuffer
import pe037.primesUnder

object pe038 {
  println("In object")
  
  def factorial(x: BigInt): BigInt = if(x == 0){1}else{x * factorial(x - 1)}
  
  def all_pandigital(): List[List[Int]] = {
    var this_index = 0
    var select_position = 0
    val pan_list = new ListBuffer[List[Int]]
    var num_list = new ListBuffer[Int]
    var this_pan_list = new ListBuffer[Int]
    val div_list = new ListBuffer[Int]
    
    for (i <- 8 to 1 by -1){
      div_list += factorial(i).toInt
    }
    
    //for (i <- 0 to (factorial(9).toInt-1)) {
    for (i <- 0 to 362879) {
      num_list = ListBuffer(9,8,7,6,5,4,3,2,1)
      this_pan_list = ListBuffer.empty
      this_index = i
      for (j <- 0 to 7){
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
  
  def is_4pandigital_mult(x: List[Int]): Boolean = {
    var this_num = x(0)*1000 + x(1)*100 + x(2)*10 + x(3)
    var next_num = x(4)*10000 + x(5)*1000 + x(6)*100 + x(7)*10 + x(8)
    2*this_num == next_num
  }
  /*
  def is_3pandigital_mult(x: List[Int]): Boolean = {
    var this_num = x(0)*1000 + x(1)*100 + x(2)*10 + x(3)
    var next_num = x(4)*10000 + x(5)*1000 + x(6)*100 + x(7)*10 + x(8)
    2*this_num == next_num
  
    var this_index = 2
    
    var next_num = this_num * 2
    var num_digits = math.floor(math.log(next_num)/math.log(10)).toInt
    if (num_digits==2){
      if not (next_num == x(this_index+1)*10 + x(this_index)){false}
    
    }
      var next_num = this_num * 2
    
    
    
    
  }
  */
 
  
  def main (args: Array[String]): Unit = {
    println("In main")
    var sum = 0L
    val pan_list = all_pandigital()
    println (pan_list(0))
    println (is_4pandigital_mult(List(9,1,1,1,1,8,2,2,2)))
    
    for(a <- 0 to pan_list.length.toInt-1){
      if(is_4pandigital_mult(pan_list(a)))
      {println(pan_list(a))}
      sum += a 
    }
    
    println("sum is: " + sum)
    //println(prime_list)
  }
}