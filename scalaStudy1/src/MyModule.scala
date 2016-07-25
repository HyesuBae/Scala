/**
  * Created by hyesubae on 16. 7. 25.
  */
object MyModule {
  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  // Unit : Java나 C의 void 와 같은 목적으로 쓰임.
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))

}
