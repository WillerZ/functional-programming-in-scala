import scala.annotation.tailrec
import scala.{Stream => _, _}

/**
 * Created by philwill on 18/01/15.
 */
object Chapter5 {

  sealed trait Stream[+A] {
    final def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // 5.1
    final def toList: List[A] = {
      @tailrec
      def _toList(sofar: List[A], s: () => Stream[A]): List[A] =
        s() match {
          case Empty => sofar
          case Cons(h, t) => _toList(sofar :+ h(), t)
        }
      _toList(Nil: List[A], () => this)
    }

    // 5.2 - Not tail recursive
    final def take(n: Int): Stream[A] = {
      def _take(s: () => Stream[A], n: Int): Stream[A] = if (n < 0) ???
      else if (n == 0) Stream.empty[A]
      else
        s() match {
          case Empty => ???
          case Cons(h, t) => Cons(h, () => _take(t, n - 1))
        }
      _take(() => this, n)
    }

    final def drop(n: Int): Stream[A] = {
      @tailrec
      def _drop(s: () => Stream[A], n: Int): Stream[A] = if (n < 0) ???
      else if (n == 0) s()
      else
        s() match {
          case Empty => ???
          case Cons(h, t) => _drop(t, n - 1)
        }
      _drop(() => this, n)
    }

    // 5.3
    final def takeWhile(p: A => Boolean): Stream[A] = {
      def _take(s: () => Stream[A], p: A => Boolean): Stream[A] =
        s() match {
          case Cons(h, t) if (p(h())) => Cons(h, () => _take(t, p))
          case _ => Stream.empty[A]
        }
      _take(() => this, p)
    }

    final def exists(p: A => Boolean): Boolean = {
      @tailrec
      def _exists(s: Stream[A], p: (A) => Boolean): Boolean = s match {
        case Cons(h, t) if (p(h())) => true
        case Cons(h, t) => _exists(t(), p)
        case Empty => false
      }
      _exists(this, p)
    }

    final def foldLeft[B](z: => B)(f: (=> B, A) => B): B = {
      @tailrec
      def _foldLeft(s: () => Stream[A], z: => B)(f: (=> B, A) => B): B = s() match {
        case Empty => z
        case Cons(h, t) => _foldLeft(t, f(z, h()))(f)
      }
      _foldLeft(() => this, z)(f)
    }

    final def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

    // 5.4
    final def forAll(p: (A) => Boolean): Boolean = {
      @tailrec
      def _forAll(s: Stream[A], p: (A) => Boolean): Boolean = s match {
        case Cons(h, t) if (p(h())) => _forAll(t(), p)
        case Empty => true
        case _ => false
      }
      _forAll(this, p)
    }

    // 5.5
    final def takeWhileRight(p: A => Boolean): Stream[A] = this.foldRight[Stream[A]](Stream.empty[A])((a, b) => if (p(a)) Cons(() => a, () => b) else Stream.empty[A])

    // 5.6
    final def headOptionRight: Option[A] = this.foldRight[Option[A]](None)((a, b) => Some(a))

    // 5.7
    final def map[B](f: A => B): Stream[B] = this.foldRight(Stream.empty[B])((a, b) => Cons(() => f(a), () => b))

    final def filter(f: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])((a, b) => if (f(a)) Cons(() => a, () => b) else b)

    final def append[B >: A](others: => Stream[B]): Stream[B] = this.foldRight[Stream[B]](others)((a, b) => Cons(() => a, () => b))

    final def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight[Stream[B]](Stream.empty[B])((a, b) => f(a).append(b))

    // 5.13
    final def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold[B, () => Stream[A]](() => this)((a) => a() match {
      case Cons(h, t) => Some(f(h()), t)
      case Empty => None
    })

    final def takeUnfold(n: Int): Stream[A] = Stream.unfold[A, (Int, () => Stream[A])]((n, () => this))((a) => {
      val n = a._1
      val s = a._2
      if (n < 0) ???
      else if (n == 0) None
      else
        s() match {
          case Empty => ???
          case Cons(h, t) => Some(h(), (n - 1, () => t()))
        }
    })

    final def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(() => this)((a) => {
      a() match {
        case Cons(h, t) if (p(h())) => Some(h(), () => t())
        case _ => None
      }
    })

    final def zipWith[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold[C, (() => Stream[A], () => Stream[B])](() => this, () => bs)((a) => {
      val as = a._1
      val bs = a._2
      as() match {
        case Cons(ah, at) => bs() match {
          case Cons(bh, bt) => Some(f(ah(), bh()), (at, bt))
          case _ => None
        }
        case _ => None
      }
    })

    final def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold[(Option[A], Option[B]), (() => Stream[A], () => Stream[B])](() => this, () => bs)((a) => {
      val as = a._1
      val bs = a._2
      as() match {
        case Cons(ah, at) => bs() match {
          case Cons(bh, bt) => Some((Some(ah()), Some(bh())), (at, bt))
          case _ => Some((Some(ah()), None), (at, bs))
        }
        case _ => bs() match {
          case Cons(bh, bt) => Some((None, Some(bh())), (as, bt))
          case _ => None
        }
      }
    })

    // 5.14
    final def startsWith[A](s: Stream[A]): Boolean = zipWith[A, Boolean](s)((a, b) => a == b).forAll(_ == true)

    // 5.15
    final def tails: Stream[Stream[A]] = Stream.unfold[Stream[A], Option[() => Stream[A]]](Some(() => this))((as) => {
      as match {
        case Some(a) => a() match {
          case Cons(ah, at) => Some(a(), Some(at))
          case _ => Some(a(), None)
        }
        case None => None
      }
    })

    // 5.16
    final def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
      case Cons(h, t) => {
        lazy val tail = t().scanRight(z)(f)
        Stream.cons(f(h(), tail.headOption match { case Some(b) => b case None => z}), tail)
      }
      case Empty => Stream(z)
    }

    // Can't use Stream.unfold to implement scanRight with the same performance as above because it inherently traverses
    // left-to-right and provides nowhere to stash the in-progress tail. It is possible to use the tail-recursive
    // implementation of foldLeft as follows:
    final def scanRightByFoldingLeft[B](z: => B)(f: (A, => B) => B): Stream[B] = {
      this.foldLeft[Stream[B] => Stream[B]]((a) => a)((bf, a) => (b: Stream[B]) => {
        lazy val bbf = bf
        bbf(b match {
          case Cons(h, t) => {
            println("a")
            lazy val zz = f(a, h())
            Cons(() => zz, () => b)
          }
          case Empty => ???
        })
      })(Stream(z))
    }

    // I notice that foldLeft wasn't in the book or in any exercise so here it is with foldRight:
    final def scanRightByFoldingRight[B](z: => B)(f: (A, => B) => B): Stream[B] = {
      // Makes foldRight behave like foldLeft; we saw this in Chapter3
      def fl[A, B](as: Stream[A], z: B)(f: (B, A) => B): B = {
        as.foldRight((b: B) => b)((a, g) => (b) => {
          lazy val gf = g; gf(f(b, a))
        })(z)
      }
      fl[A, Stream[B] => Stream[B]](this, (a) => a)((bf, a) => (b: Stream[B]) => {
        lazy val bbf = bf
        bbf(b match {
          case Cons(h, t) => {
            println("a")
            lazy val zz = f(a, h())
            Cons(() => zz, () => b)
          }
          case Empty => ???
        })
      })(Stream(z))
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = t
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // 5.8
    def constant[A](c: A): Stream[A] = {
      lazy val co: Stream[A] = Stream.cons(c, co)
      co
    }

    // 5.9
    def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

    // 5.10
    def fibs: Stream[Int] = {
      def _fibs(n1: Int, n2: Int): Stream[Int] = {
        Stream.cons(n1, _fibs(n2, n1 + n2))
      }
      _fibs(0, 1)
    }

    // 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      val hOption = f(z)
      hOption match {
        case None => Stream.empty[A]
        case Some(t) => cons(t._1, unfold(t._2)(f))
      }
    }

    // 5.12
    def ones: Stream[Int] = unfold(1)((a) => Some(a, a))

    def fibsUnfold: Stream[Int] = unfold[Int, (Int, Int)]((0, 1))((ns) => Some(ns._1, (ns._2, ns._1 + ns._2)))

    def fromUnfold(n: Int): Stream[Int] = unfold[Int, Int](n)((n) => Some(n, n + 1))

    def constantUnfold[A](n: A): Stream[A] = unfold[A, A](n)((n) => Some(n, n))
  }

}
