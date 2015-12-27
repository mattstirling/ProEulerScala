import scala.collection.mutable.ListBuffer

object pe043 {
  println("In Object")

  def factorial(x: BigInt): BigInt = if(x == 0){1}else{x * factorial(x - 1)}
  
  def this_09pandigital(index:Int,div_list:List[Int]):List[Int] = {
    var select_position = 0
    var this_index = index
    var num_list = ListBuffer(9,8,7,6,5,4,3,2,1)
    var this_pan_list = new ListBuffer[Int] 
    this_pan_list = ListBuffer.empty
    for (j <- 0 to 8){
      if(j==1){num_list+=0}
      select_position = math.floor(this_index/div_list(j)).toInt
      //println(i + "," + j + "," + this_index + "," + select_position + "," + num_list(select_position))
      this_index -= select_position * div_list(j)
      this_pan_list += num_list(select_position)
      //println(this_pan_list)
      num_list -= num_list(select_position)
    }
    this_pan_list += num_list(0) 
    //println(this_pan_list)
    this_pan_list.toList
      
  }
  
  def all_09pandigital_stream(): Stream[List[Int]] = {
    var this_index = 0
    var select_position = 0
    val pan_list = new ListBuffer[List[Int]]
    var num_list = new ListBuffer[Int]
    var this_pan_list = new ListBuffer[Int]
    val div_list = new ListBuffer[Int]
    
    for (i <- 9 to 1 by -1){
      div_list += factorial(i).toInt
    }
    //println(div_list)
    
    Stream.from(0).takeWhile(_ < (9*factorial(9).toInt)-1).map(n => this_09pandigital(n,div_list.toList))
    
    //pan_list.toList
  }
  
  
  
  def all_09pandigital(): List[List[Int]] = {
    var this_index = 0
    var select_position = 0
    val pan_list = new ListBuffer[List[Int]]
    var num_list = new ListBuffer[Int]
    var this_pan_list = new ListBuffer[Int]
    val div_list = new ListBuffer[Int]
    
    for (i <- 9 to 1 by -1){
      div_list += factorial(i).toInt
    }
    //println(div_list)
    
    for(i <- 0 to (9*factorial(9).toInt)-1) {
      num_list = ListBuffer(9,8,7,6,5,4,3,2,1)
      this_pan_list = ListBuffer.empty
      this_index = i.toInt
      //println(i + "," + "")
      for (j <- 0 to 8){
        if(j==1){num_list+=0}
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
  
  def get_3len_subnumber(number_list: List[Int],start:Int):Int = {
    /*
    d2d3d4=406 is divisible by 2
    d3d4d5=063 is divisible by 3
    d4d5d6=635 is divisible by 5
    d5d6d7=357 is divisible by 7
    d6d7d8=572 is divisible by 11
    d7d8d9=728 is divisible by 13
    d8d9d10=289 is divisible by 17
     */
    100*number_list(start-1) + 10*number_list(start) + number_list(start+1)
  }
  
  def is_pe043_divisible(number_list: List[Int]):Boolean = {
    var result = true
    val div_list = List(0,0,2,3,5,7,11,13,17)
    for (i<-2 to 8){
      if(get_3len_subnumber(number_list,i)%div_list(i)>0){result = false}
    }
    result
  }
  
  def list_09dig_to_num(n_list:List[Int]):Long = {
    var sum =0L
    for(i<-0 to 9){
      sum += n_list(i) * math.pow(10,9-i).toLong
    }
    sum
  }
  
  
  
  def main (args: Array[String]): Unit = {
    println("In main")
    
    //get all 09pandigital numbers
    val pan_list = all_09pandigital_stream()
    var sum = 0L
    var count = 0
    //get all len-3 subnumbers
    //test each len-3 subnumber if it is divisible
    //store/sum each 09pandigital if it passes all tests
    
    
    for(a <- pan_list){
      //println(list_09dig_to_num(a))
      if(is_pe043_divisible(a)){
        println(list_09dig_to_num(a))
        sum += list_09dig_to_num(a)
        count += 1
      }
    }
    println("count is: " + count)
    println("sum is: " + sum)
  }
}