import PE_Lib.is_coprime


object pe233_try2 {
  
  def is_int_coord(x:Double,y:Double):Boolean = {
    y%1==0  
  }
  //You could simply do this: d % 1 == 0 to see if some double d is whole.
  
  def is_whole(y:Double):Boolean = {
    y%1==0  
  }
  
  def main (args: Array[String]): Unit = {
    println("In Main")
    val start_time = System.nanoTime
    val h = new collection.mutable.HashMap[Int, Int]
    var this_N = 0.1d
    var keep_this_N = 0
    
    for(i <- 10 to 10000){
      for(j <- 10 to 10000){
        this_N = (i*i+j*j).toDouble/(i*j).toDouble
        if(is_whole(this_N)){
          keep_this_N = this_N.toInt
          println(keep_this_N)
          //store this value for N
          if(! h.contains(keep_this_N)){
            h(keep_this_N) = 0
          }
          h(keep_this_N) = h(keep_this_N) + 1
        }
        //println(i+","+j+","+this_N)  
      }
    }
    
    println( h.get(10))
    
    //old_main()
    val end_time = System.nanoTime
    println("run time is : " + (end_time - start_time)/1e6+"ms")
  }
    
  def old_main(): Unit = {  
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