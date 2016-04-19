import PE_Lib.is_coprime

object pe540 {
  
  
  
  
  def main (args: Array[String]): Unit = {
    println("In Main")
    var cum_count = 0L
    var a = 1L
    var b = 2L
    var c = 0L
    var n = 1000000L
    while(a<n){
      while(b<n){
        c = math.floor(math.sqrt(a*a + b*b)).toLong
        //println(a+","+b+","+c)
        if(c<n && c*c == a*a + b*b){
          if(is_coprime(a,b) && is_coprime(a,c) && is_coprime(b,c)){
            //println(a+","+b+","+c+" pass")
            cum_count += 1  
          }
        }
        b+=1
      }
      a+=1
      b=a+1
    }
    println("count is: " + cum_count)
  }
}