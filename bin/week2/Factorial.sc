package week2

object Factorial {
  println("Welcome to the Scala worksheetdfa") 
   
  println("Welcome to the Scala worksheet")  
    
  
   
  def factorial(n:Int): Int ={
  	if(n == 0) 1 else n * factorial(n - 1)
  }
  
  factorial(2)
}