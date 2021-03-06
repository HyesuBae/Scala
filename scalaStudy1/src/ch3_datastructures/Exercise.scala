package ch3_datastructures

/**
  * Created by hyesubae on 16. 8. 7.
  */
object Exercise {
  def main(args: Array[String]): Unit = {
    exercise1()
    println(List.drop(List(1,2,3,4,5),2))
    println(List.append(List("Hello", "world"), List("!","!")))
    println(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_)))
    println(List.sum2(List(1,2,3)))
    println(List.length(List(1,2,3,4,6,8,9,10,11,12,13,14,15,16,17,20,23,25,27,28,30,31,32)))
    println(List.reverse(List(1,2,3)))
    println(List.append2viaFoldLeft(List(1,2,3), List(4,5,6)))
    println(List.append2viaFoldRight(List(1,2,3), List(4,5,6)))
    println(List.addOne(List(1,2,3)))
    println(List.filter(List(1,3,5,2,4))(_ % 2 == 0))
    println(List.filter2(List(1,3,5,2,4))(_ % 2 == 0))
  }

  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  // x는 3. 세번째 case에서 match됨.
  // 네번째, 다섯번째도 match되긴 하지만 세번째 케이스에서 매칭돼서 리턴.
  def exercise1(): Unit = {
    val a = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4,_))) => x
      case Nil => 42
      case Cons(x, (Cons(y, Cons(3, Cons(4,_))))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(a)
  }

}
