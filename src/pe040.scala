

object pe040 {
  println("In object")
  
  def num_digits(x:Int):Int = 1+math.floor(math.log(x)/math.log(10)).toInt
  
  def nth_digit(x:Int, n:Int):Int = {
    if(n==1){(x%10).toInt}
    else{(((x-x%math.pow(10,n-1))/math.pow(10,n-1))%10).toInt}
  }
  
  def main (args: Array[String]): Unit = {
    println("In main")
    val index_list = List(1,10,100,1000,10000,100000,1000000,2000000000)
    var index_num = 0
    var cum_num_digits = 0
    var within_num_index = 0
    var index_mult = 1 * 1 
    var this_digit = 0
    
    var this_string = ""
   
    
    for(i<- 1 to 190000){
      
      this_string += i
      
      cum_num_digits += num_digits(i)
      
      if (cum_num_digits >= index_list(index_num)){
        within_num_index = 1+cum_num_digits-index_list(index_num)
        this_digit = nth_digit(i,within_num_index)
        index_mult = index_mult * this_digit
        println(List(index_num,cum_num_digits,i,this_digit,within_num_index,index_mult))
        index_num +=1
        
      }
    }  
    
    for(i<-List(1,10,100,1000,10000,100000,1000000)){
      println(this_string.charAt(i-1))
    }
    
  
  }
}