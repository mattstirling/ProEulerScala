import scala.collection.mutable.ArrayBuffer
import _root_.scala.collection.JavaConversions._
import java.util.{Collections, List => JList}
import java.util.{Collections, Arrays => JArrays}

object pe045 {
  
  class SearchableSeq[T](a: Seq[T])(implicit ordering: Ordering[T]) {
    val list: JList[T] = a.toList
    def binarySearch(key: T): Int = Collections.binarySearch(list, key, ordering)
  }
  
  class ObjectArrayTools[T <: Long](a: Array[T]) {                  
    def binarySearch(key: T) = {
      java.util.Arrays.binarySearch(a.asInstanceOf[Array[Long]],key)
    }
  }
  
  implicit def seqToSearchable[T](a: Seq[T])(implicit ordering: Ordering[T]) = new SearchableSeq(a)(ordering)
  
  implicit def anyrefarray_tools[T <: Long](a: Array[T]) = new ObjectArrayTools(a)
  
  def pent_num_list(max_val:Long): Array[Long] = {
    var num_array = new ArrayBuffer[Long]
    var this_val = 0L
    var i = 0L
    while(this_val<max_val){
      i+=1
      this_val = (i*(3*i-1)/2).toLong
      num_array += this_val
    }
    num_array.toArray
  }
  
  def tri_num_list(max_val:Long): List[Long] = {
    var num_array = new ArrayBuffer[Long]
    var this_val = 0L
    var i = 0L
    while(this_val<max_val){
      i+=1
      this_val = (i*(i+1)/2).toLong
      num_array += this_val
    }
    num_array.toList
  }
  
  def hex_num_list(max_val:Long): List[Long] = {
    var num_array = new ArrayBuffer[Long]
    var this_val = 0L
    var i = 0L
    while(this_val<max_val){
      i+=1
      this_val = (i*(2*i-1)).toLong
      num_array += this_val
    }
    num_array.toList
  }
  
  def main (args: Array[String]): Unit = {
    val max_val = math.pow(10,12).toLong
    val pent_list = pent_num_list(max_val).toArray
    val tri_list = tri_num_list(max_val).toArray
    val hex_list = hex_num_list(max_val).toArray
    println("have lists")
    
    for (i <- 1 to hex_list.length-1){
      if(pent_list.binarySearch(hex_list(i))>0 && tri_list.binarySearch(hex_list(i))>0){
      //if(pent_list.find(_==hex_list(i)).isDefined && hex_list.find(_==hex_list(i)).isDefined){
        println(hex_list(i))
      }
    }
  }
}