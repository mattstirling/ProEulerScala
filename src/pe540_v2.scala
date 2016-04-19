import PE_Lib.is_coprime

object pe540v2 {
  
  def is_perfect_square(n:Long):Boolean = {
    val this_square_root = math.floor(math.sqrt(n)).toLong
    this_square_root*this_square_root==n
  } 
  
  
  def main (args: Array[String]): Unit = {
    /*
     * a = m^2 - n^2
     * b = 2 m n 
     * c = m^2 + n^2
     * 
     * a,b,c are a primitive triple iff m,n coprime and m-n is odd
     * 
     * to confirm P 10^6 = 159139
     * 
     */
    println("In Main")
    val start_time = System.nanoTime
    var cum_count = 0L
    var m = 1L
    var n = 2L
    var a = 0L
    var b = 0L
    var c = 0L
    var max_amt = math.pow(10,10).toLong
    //var max_amt = 3141592653589793L
    var limit_amt = max_amt 
    while(m<math.floor(math.sqrt(max_amt/2)).toLong){
      n=m+1
      while(n<=math.floor(math.sqrt(max_amt - m*m)).toLong){
        if(is_coprime(m,n)&&(n-m)%2==1){
          cum_count += 1
          //a = n*n - m*m
          //b = 2*m*n
          //c = n*n + m*m
          
          /*
          if(is_perfect_square(((c-a)*(c-b)/2).toLong)==false){
            println("("+a+","+b+","+c+") "+(c*c == a*a + b*b)+" perfect square test.")
          }
          */
          
          /*
          if(c <= max_amt){
            //println("("+a+","+b+","+c+") "+(c*c == a*a + b*b))
            cum_count += 1
          }
          */
        }
        n+=2 //lets us consider only odd n for even m, or only even n for odd m 
      }
      m+=1
    }
    println("done.")
    println("count is: " + cum_count)
    val end_time = System.nanoTime
    println("run time is : " + (end_time - start_time)/1e6+"ms")
  }
}