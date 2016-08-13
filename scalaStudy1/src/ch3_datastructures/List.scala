package ch3_datastructures

/**
  * Created by hyesubae on 16. 8. 7.
  */

// sealed: 이 trait에 대한 모든 구현은 반드시 이 파일 안에 선언되어야 함.
// trait : 자바의 interface와 비슷.
// +     : A가 List의 covariant(공변) 매개변수임을 뜻하는 variance annotation(가변 지정자)
//         예를 들어 Dog이 Animal의 subtype이면 List[Dog]도 List[Animal]의 subtype으로 간주되는 것.
sealed trait List[+A]
// Nothing은 모든 타입의 하위타입이고 A가 covariant하므로 Nil은 List[Double], List[Int]등 모든 타입의 리스트로도 간주될 수 있다.
case object Nil extends List[Nothing]
case class Cons[+A](head: A, trail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match{
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match{
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2    // a1의 마지막 원소까지 간 다음 그 뒤에 a2를 붙임.
    case Cons(a, as) => Cons(a, append(as, a2))
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match{
    case Nil => Nil
    case Cons(x, xs) => xs  // case문의 결과 안에서 그 값이 무시되는 변수는 _를 사용하는 것이 일반적. 여기서는 x를 _로 표현하면 됨.
  }

  // Exercise 3.3
  def setHead[A](as: List[A], a: A): List[A] = as match{
    case Nil => Cons(a, Nil)
    case Cons(x, xs) => Cons(a, xs) //여기서도 실제로 x값이 쓰이지 않으므로 x를 _로 표현하는 게 나음.
  }

  // Exercise 3.4 (못풀었음 ㅠㅠ)
  // 내가 접근한 방법: match를 사용하고 함수 안에 또 함수를 정의하려고 했는데
  // match 블락 안에서는 case문만 나와야 하는 건가보다
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match{
      case Nil => Nil
      case Cons(_, as) => drop(as, n-1)
    }
  }

  // Exercise 3.5 (못풀었음ㅠㅠ)
  // case에서 if문도 쓸 수 있다!
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Cons(a, as) if f(a) => dropWhile(as, f)
    case _ => l
  }

  // Exercise 3.6 (못풀었음ㅠㅠ)
  def init[A](l: List[A]): List[A] = l match{
    case Nil => Nil
    case (_, Nil) => Nil  // 마지가 원소 하나에 대해서 바로 Nil을 리턴해줌으로써 마지막 원소를 제거하는 셈.
    case Cons(a, as) => Cons(a, init(as))
  }

}
