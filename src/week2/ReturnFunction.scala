package week2

object ReturnFunction {

  def sum(f:Int => Int): (Int, Int) => Int = {
    def sumF(a:Int, b:Int):Int={
      if (a>b) 0
      else f(a) + sumF(a+1,b)
    }
    sumF
  }

  def sumTail(f:Int => Int): (Int, Int) => Int = {
    def sumF(a:Int, b:Int):Int={
      def loop(acc:Int,a:Int):Int = {
	      if (a>b) acc
	      else loop(acc + f(a), a + 1)
      }
      loop(0,a)
    }
    
    sumF
  }

  def sumTail2(f:Int => Int)(a:Int, b:Int): Int = {
    
	  def loop(acc:Int,a:Int):Int = {
	      if (a>b) acc
	      else loop(acc + f(a), a + 1)
	  }
	  loop(0,a)
    
  }
  
  def sum2(f:Int => Int)(a:Int, b:Int): Int = {
      if (a>b) 0
      else f(a) + sum(f)(a+1,b)
  }
  
  def fact = (f:Int) => f
  def sumCubes = sumTail(x => x * x * x)
  def sumFactorials = sumTail(fact)
  
  
  
  def main(args: Array[String] ) {
    
    val s = sum(x => x)
    var r = s(4, 6)
    assert(r == 15)
    println(r);

    val s2 = sumTail(x => x)
    val r2 = s2(4, 6)
    println(r2);
    assert(r2 == 15)

    val r3 = sumTail(x => x)(4,6)
    println(r3);
    assert(r3 == 15)

    r = sumTail2(x => x)(4,6)
    println(r);
    assert(r == 15)
    
    println("Done!")
    
    val v = sumCubes(1, 10) + sumFactorials(10,20)
    
    println("Done!" + v)
  }
  
}