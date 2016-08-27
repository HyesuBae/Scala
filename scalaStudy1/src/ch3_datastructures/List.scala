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
  def dropWhile1[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Cons(a, as) if f(a) => dropWhile1(as, f)
    case _ => l
  }


  // dropWhile1에서는 f에서 parameter의 형식을 명시해야하지만 이 버전에서는 명시하지 않아도 됨.
  // 즉 dropwhile1은 dropWhile1(List(1,2,3), (x: Int) => x < 2) 처럼 x의 타입을 지정해야하지만
  // dropWhile2는 dropWhile2(List(1,2,3))(x => x < 2) 처럼 x의 타입을 지정하지 않아도 됨.
  // dropWhile2(List(1,2,3))은 하나의 함수를 돌려주며 그 함수에 다시 f를 argument로 전달한다.
  //    => "dropWhile2는 커링되었다."
  def dropWhile2[A](l: List[A])(f: A => Boolean): List [A] =  l match{
    case Cons(a, as) if f(a) => dropWhile2(as)(f)
    case _ => l
  }

  // Exercise 3.6 (못풀었음ㅠㅠ)
  //def init[A](l: List[A]): List[A] = l match{
  //  case Nil => Nil
  //  case (_, Nil) => Nil  // 마지가 원소 하나에 대해서 바로 Nil을 리턴해줌으로써 마지막 원소를 제거하는 셈.
  //  case Cons(a, as) => Cons(a, init(as))
  //}


  //////////////// 3.4

  // 위의 sum, product 함수에서는 입력 리스트가 Nil일 때 Nil을 리턴해주는 부분이 중복된다.
  // 이 중복되는 부분을 대체하기 위한 함수. 다만 리턴 값은 z로 지정하여 sum과 product가 동일한 값을 리턴할 필요는 없도록 함.

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match{
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x,y) => x + y)

    //3.8
    //foldRight(ns, Nil:List[Int])(Cons(_,_))
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)   // (_ * _)는 (x, y) => x* y를 간결하게 표현한 것.
    // 스칼라가 x와 y의 형식을 추론할 수 있다면 익명함수 (x,y) => x + y를 _ + _ 로 표현할 수 있다.
    // 이는 함수 parameter가 본문 안에서 한 번씩만 언급될 떄 유용한 단축 표기법이다.
  }

  // Exercise 3.9 (못 풀었음)
  // (_, n) => n + 1이 어떻게 길이 계산이 되는 거지...? n이 List의 원소 중 하나가 아니라
  // foldRight에서 z값인가?ㅠㅠ
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, n) => n + 1)
  }

  // Exercise 3.10
  // 못 풀었음 ^ㅠ^
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match{
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  //Exercise 3.11
  def sum3(as: List[Int]) = {
    foldLeft(as, 0)(_ + _)
  }

  def product3(as: List[Double]) = {
    foldLeft(as, 1.0)(_ * _)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((n, _) => n + 1)
  }

  //Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((xs, x) => Cons(x, xs))
  }

  // Exercise 3.13 => 일단 패스

  // Exercise 3.14
  def append2viaFoldLeft[A](as1: List[A], as2: List[A]): List[A] ={
    foldLeft(reverse(as1), as2)((xs, x) => Cons(x, xs))
  }

  def append2viaFoldRight[A](as1: List[A], as2: List[A]): List[A] = {
    foldRight(as1, as2)((x, xs) => Cons(x, xs))   // x와 xs는 f 안에서 한번씩만 순서대로 사용되므로 Cons(_, _)로 표현 가능.
  }

  // Exercise 3.15 => 일단 패스

  // Exercise 3.16
  def addOne(ints: List[Int]): List[Int] = ints match{
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, addOne(xs))
  }

  // 논리는 맞았는데 Nil의 타입을 List[Int]로 지정해야하는지 몰랐음.
  // 왜 지정해야할까?
  def addOne2(ints: List[Int]): List[Int] = {
    foldRight(ints, Nil: List[Int])((x, xs) => Cons(x+1, xs))
  }

  // Exercise 3.17
  def doubleToString(ds: List[Double]): List[String] = ds match{
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def doubleToString2(ds: List[Double]): List[String] = {
    foldRight(ds, Nil: List[String])((x, xs) => Cons(x.toString, xs))
  }
}
