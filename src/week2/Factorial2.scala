package week2

object Factorial2 {

  def factorial(n:Int): Int ={
	  def loop(acc:Int, n:Int): Int = 
	    if (n == 0) acc 
	    else 
	    loop(acc * n, n-1)
    
    loop(1,n)
  }
  
  def main(args:Array[String]) {
      var result = factorial(3)
	  assert(result == 6) 
	  result = factorial(4)
	  assert(result == 24)
	  print("Done!")
  }

}