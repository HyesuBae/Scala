package chapter2_gettingstarted

/**
  * Created by hyesubae on 16. 7. 30.
  */
object MyModule {
  def main(args: Array[String]): Unit ={
    println(formatResult("absolute", -35, abs))
    println(formatResult("factorial", 6, factorial))
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
}
