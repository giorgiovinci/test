package week2

object Sum {

  def sumLinearRecursion(a:Int, b:Int): Int = {
    if (a > b) 0 else a + sumLinearRecursion(a + 1, b)
  }

  def sumLinearRecursion2(f:Int => Int, a:Int, b:Int): Int = {
		  if (a > b) 0 else f(a) + sumLinearRecursion(a + 1, b)
  }
  
  def sumTailRecursion(f:Int => Int, a:Int, b:Int): Int ={
    def loop(f:Int => Int, acc:Int, a:Int, b:Int):Int = {
      if (a >= b) acc 
      else loop(f, acc + f(a + 1), a+1, b)
    }
    loop(f:Int => Int, a, a,b)
    
  }
  
  def sumTailRecursion2(f:Int => Int, a:Int, b:Int): Int ={
    def loop(a:Int, acc:Int):Int = {
      if (a >= b) acc 
      else loop(a+1, acc + f(a + 1))
    }
    loop(a, a)    
  }

    def sumTailRecursion3(f:Int => Int, a:Int, b:Int): Int ={
    def loop(a:Int, acc:Int):Int = {
      if (a > b) acc 
      else loop(a+1, acc + f(a))
    }
    loop(a, 0)    
  }
  
  
  def main(args: Array[String] ) {
    var r = sumLinearRecursion(4, 6)
    assert(r == 15)
    println(r);

    r = sumLinearRecursion2((x:Int)=> x, 4, 6)
    println(r);
    assert(r == 15)

    r = sumTailRecursion((x:Int)=> x, 4, 6)
    println(r);
    assert(r == 15)
    
    r = sumTailRecursion2((x:Int)=> x, 4, 6)
    println(r);
    assert(r == 15)

    r = sumTailRecursion3((x:Int)=> x, 4, 6)
    println(r);
    assert(r == 15)
    
    println("Done!")
  }
  
}