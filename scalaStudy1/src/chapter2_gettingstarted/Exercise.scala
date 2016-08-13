package chapter2_gettingstarted

/**
  * Created by hyesubae on 16. 7. 30.
  */
object Exercise {
  def main(args: Array[String]): Unit = {
    println(Exercise.fib(6))
    println(Exercise.fib2(6))
    print(isSorted(Array(3,4,5), (a:Int, b:Int) => a==b))
  }

  // Exercise 2.1  T_T
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

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean ={
    def loop(n: Int) : Boolean ={
      if (n >= as.length -1) true    // 땡! as.length-1 까지 계산해야함. 아래 else if에서 array의 n+1값도 조회하니까.
      else if(ordered(as(n), as(n+1))) false
      else loop(n+1)
    }
    loop(0)
  }

  // Exercise 2.3
  def curry[A,B,C](f:(A,B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)    // a => b => f(a,b) 로 표현 가능. 타입 추론이 가능하기 때문인가!?
  }

  // Exercise 2.4
  def uncurry[A,B,C](f: A => B => C) : (A, B) => C = {
    (a: A, b: B) => f(a)(b)       // (a,b) => f(a)(b) 로 표현 가
  }

  // Exercise 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C ={
    (a: A) => f(g(a))             // a => f(g(a))로 표현 가능
  }
}
