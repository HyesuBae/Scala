package chapter2_gettingstarted

/**
  * Created by hyesubae on 16. 7. 30.
  */
object Exercise1 {
  def fib(n: Int) : Int = {
    def go(n: Int) : Int = {
      if (n == 0) 0
      else if (n == 1) 1
      else go(n-1) + go(n-2)
    }

    go(n-1)
  }

  def fib2(n:Int): Int = {
    def go(n: Int, prev: Int, cur: Int) : Int = {
      if (n == 1) prev
      else go(n-1, cur, prev+cur)
    }

    go(n, 0, 1)
  }

  def main(args: Array[String]): Unit ={
    println(Exercise1.fib(6))
    println(Exercise1.fib2(6))
  }
}
