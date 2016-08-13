package chapter2_gettingstarted

/**
  * Created by hyesubae on 16. 7. 30.
  */
object MyModule {
  def main(args: Array[String]): Unit ={
    println(formatResult("absolute", -35, abs))
    println(formatResult("factorial", 6, factorial))
    println( findFirst(Array(7,9,13), (x: Int) => x == 14))


  }
  def abs(n: Int): Int ={
    if(n < 0) -n
    else n
  }

  def factorial(n: Int): Int ={
    def go(n: Int, acc: Int): Int ={
      if (n<=0) acc
      else go(n-1, acc*n)
    }
    go(n, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst[A](as: Array[A], p:A => Boolean): Int ={
    @annotation.tailrec
    def loop(n: Int) : Int = {
      if(n >= as.length) -1
      else if(p(as(n))) n
      else loop(n+1)
    }
    loop(0)
  }

  def partial1[A,B,C](a: A, f: (A,B)=>C): B => C ={
    (b: B) => f(a,b)
  }
}
