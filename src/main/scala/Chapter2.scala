import scala.annotation.tailrec

/**
 * Created by philwill on 17/01/15.
 */
object Chapter2 {
  //2.1
  def fib(n: Int): Int = {
    @tailrec
    def fibForward(fibN_1: Int, fibN_2: Int, n: Int): Int = {
      if (n == 0) fibN_2
      else fibForward(fibN_2, fibN_1 + fibN_2, n - 1)
    }
    if (n == 0) 0
    else if (n > 0) fibForward(0,1,n-1)
    else ???
  }
  //2.2
  @tailrec
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    as match {
      case Array(a1,a2,_*) => ordered(a1,a2) && isSorted(as.tail,ordered)
      case _ => true
    }
  }
  @tailrec
  def isSorted[A](as: List[A], ordered: (A,A) => Boolean): Boolean = {
    as match {
      case a1::a2::t => ordered(a1,a2) && isSorted(a2::t,ordered)
      case _ => true
    }
  }
  //2.3
  def curry[A,B,C](f: (A,B)=>C): A => B => C = {
    a: A => b: B => f(a, b)
  }
  //2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  //2.5
  def compose[A,B,C](f: B=>C, g: A=>B): A=>C = {
    a:A => f(g(a))
  }
}
