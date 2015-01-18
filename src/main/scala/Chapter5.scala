import scala.{Stream => _, _}

/**
 * Created by philwill on 18/01/15.
 */
object Chapter5 {

  sealed trait Stream[+A] {
    def headOption: Option[A]

    // 5.1
    def toList: List[A]

    // 5.2
    def take(n: Int): Stream[A]

    def drop(n: Int): Stream[A]

    // 5.3
    def takeWhile(p: A => Boolean): Stream[A]

    def exists(p: A => Boolean): Boolean

    def foldRight[B](z: => B)(f: (A, => B) => B): B
  }

  case object Empty extends Stream[Nothing] {
    override def headOption: Option[Nothing] = None

    // 5.1
    override def toList: List[Nothing] = Nil

    override def take(n: Int): Stream[Nothing] = ???

    override def drop(n: Int): Stream[Nothing] = ???

    // 5.3
    override def takeWhile(p: (Nothing) => Boolean): Stream[Nothing] = this

    override def exists(p: (Nothing) => Boolean): Boolean = false

    override def foldRight[B](z: => B)(f: (Nothing, B) => B): B = z
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    override def headOption: Option[A] = Some(h())

    // 5.1
    override def toList: List[A] = h() :: t().toList

    // 5.2
    override def take(n: Int): Stream[A] = if (n < 0) ??? else if (n == 0) Stream.empty[A] else Cons(h, () => t().take(n - 1))

    override def drop(n: Int): Stream[A] = if (n < 0) ??? else if (n == 0) this else t().drop(n - 1)

    // 5.3
    override def takeWhile(p: (A) => Boolean): Stream[A] = if (p(h())) Cons(h, () => t().takeWhile(p)) else t().takeWhile(p)

    override def exists(p: (A) => Boolean): Boolean = p(h()) || t().exists(p)

    override def foldRight[B](z: => B)(f: (A, B) => B): B = f(h(), t().foldRight(z)(f))
  }

  object Stream {
    def cons[A](h: () => A, t: () => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(head, tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(() => as.head, () => apply(as.tail: _*))
  }

}
