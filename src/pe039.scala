

object pe039 {
  println("In object")
  
  def main (args: Array[String]): Unit = {
    println("In main")
    var c = 0
    var max_p_count = 0
    var max_p = 0
    var p_count = 0
    
    
    for(p <- 840 to 840){
      p_count = 0
      for(a <- 1 to p-2){
        for(b<- a to p-1-a){
          c=p-a-b
          if (a*a + b*b == c*c && c>=b){
            println(List(p,a,b,c))
            p_count +=1
          }
        }
      }
      if(p_count > max_p_count){
        max_p_count = p_count
        max_p = p
      }
    }
    
    println("max is: " + max_p_count + " for " + max_p)
    
  }
}