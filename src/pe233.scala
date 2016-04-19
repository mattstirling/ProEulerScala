import PE_Lib.is_coprime


object pe233 {
  
  def is_int_coord(x:Double,y:Double):Boolean = {
    y%1==0  
  }
  //You could simply do this: d % 1 == 0 to see if some double d is whole.
  
  def main (args: Array[String]): Unit = {
    /*
     * let f(N) be the number of points with integer coordinates that are on a circle passing through (0,0),(0,N),(N,0),(N,N)
     * 
     * what is the sum of all positive integers N <= 10^11 s.t. f(N) = 420
     * 
     * r^2 = (x-m)^2 + (y-m)^2
     * 
     * for N, find r^2 and m
     * r_squared = 0.5*n*n
     * m = 0.5*n
     * 
     * get max/min val x 
     * 
     * for every value x, determine y and check if is whole
     * only check "y>m" (for all x!=m)
     * only check "x<0" for the case, x=m
     *    
     */
    println("In Main")
    val start_time = System.nanoTime
    var cum_count = 0L
    var cum_420_N_count = 0L
    var cum_420_count = 0L
    var r_squared = 0.1d
    var m = 0.1d
    var x_min = 0.1d 
    var x_max = 0.1d
    var i = 0.1d
    var max_i = 0.1d
    var x_try = 0.1d
    var y_try = 0.1d
    var r_squared_try = 0.1d
    val max_n = math.pow(10,4).toInt
    
    for(n<- max_n to max_n){
      r_squared = 0.5*n*n
      m = 0.5*n
      cum_count = 0L
      x_min = m - math.sqrt(r_squared)
      x_max = m + math.sqrt(r_squared)
      i = math.ceil(x_min)
      max_i = math.ceil(x_max)
      while(i<max_i){
        x_try = i
        y_try = m + math.sqrt(r_squared - (i - m)*(i - m))
        if(is_int_coord(x_try,y_try)){
          //println(i)
          println(i+","+x_try*x_try + y_try*y_try+","+2*n)
          cum_count += 2
        }
        i+=1.0
      }
      //println(n+","+cum_count)
      if(cum_count==36){
        cum_420_count +=1
        cum_420_N_count +=n
        println(cum_420_count+","+n+","+cum_420_N_count) 
      }
    }    
    
    val end_time = System.nanoTime
    println("run time is : " + (end_time - start_time)/1e6+"ms")
  }
}