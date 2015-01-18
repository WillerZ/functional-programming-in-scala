/**
 * Created by philwill on 18/01/15.
 */

import scala.{Option => _, Some => _, None => _, Either => _, Left => _, Right => _, _}

object Chapter4 {

  // 4.1
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]

    def getOrElse[B >: A](default: => B): B

    def orElse[B >: A](ob: => Option[B]): Option[B]

    def flatMap[B](f: A => Option[B]): Option[B]

    def filter(f: A => Boolean): Option[A]
  }

  case class Some[+A](value: A) extends Option[A] {
    override def map[B](f: (A) => B): Option[B] = Some[B](f(value))

    override def flatMap[B](f: A => Option[B]): Option[B] = f(value)

    override def getOrElse[B >: A](default: => B): B = value

    override def orElse[B >: A](ob: => Option[B]): Option[B] = this

    override def filter(f: (A) => Boolean): Option[A] = if (f(value)) this else None
  }

  case object None extends Option[Nothing] {
    override def map[B](f: (Nothing) => B): Option[B] = None

    override def flatMap[B](f: (Nothing) => Option[B]): Option[B] = None

    override def getOrElse[B](default: => B): B = default

    override def orElse[B](ob: => Option[B]): Option[B] = ob

    override def filter(f: (Nothing) => Boolean): Option[Nothing] = this
  }

  // 4.2 - Seemed to be minimal gain to using Option here, unless the xs were a Seq[Option[Double]] instead.
  def total(xs: Seq[Option[Double]]): Option[Double] = xs.fold(Some(.0): Option[Double])((a, b) => a.flatMap((v) => b.map((w) => v + w)))

  def _size(xs: Seq[Option[Double]]): Option[Int] = if (xs.isEmpty) None else Some(xs.size)

  def _mean(xs: Seq[Option[Double]]): Option[Double] = {
    total(xs).flatMap((a) => _size(xs).map((s) => a / s))
  }

  def variance(xs: Seq[Option[Double]]): Option[Double] = {
    val size: Option[Int] = _size(xs)
    val mean: Option[Double] = _mean(xs)
    val individual: Seq[Option[Double]] = xs map ((x) => x.flatMap((x) => size.flatMap((s) => mean.map((m) => math.pow(x - m, 2) / s))))
    individual.fold(Some(.0))((a, b) => a.flatMap((v) => b.map((w) => v + w)))
  }

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap((a) => b.map((b) => f(a, b)))

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil: List[A]))((a, someas) => someas match {
    case None => None
    case Some(t) => a.map((a) => a :: t)
  })

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil: List[B]))((a, somebs) => somebs match {
    case None => None
    case Some(t) => f(a).map((b) => b :: t)
  })

  def sequenceTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse[Option[A], A](a)((a) => a)

  // 4.6
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B]

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

    def orElse[EE >: E, B >: A](b: Either[EE, B]): Either[EE, B]

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
  }

  case class Left[+E](value: E) extends Either[E, Nothing] {
    override def map[B](f: (Nothing) => B): Either[E, B] = this

    override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this

    override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]): Either[EE, B] = this

    override def orElse[EE >: E, B >: Nothing](b: Either[EE, B]): Either[EE, B] = b
  }

  case class Right[+A](value: A) extends Either[Nothing, A] {
    override def map[B](f: (A) => B): Either[Nothing, B] = Right(f(value))

    override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b match {
      case Right(b) => Right(f(value, b))
      case Left(e) => Left(e)
    }

    override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]): Either[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](b: Either[EE, B]): Either[EE, B] = this
  }

  // 4.7
  def traverse[A, B,E](a: List[A])(f: A => Either[E,B]): Either[E,List[B]] =
    a.foldRight[Either[E,List[B]]](Right(Nil: List[B]))((a, somebs) => somebs match {
    case Left(e) => {
      val e2 = f(a)
      e2 match {
        case Left(e2) => Left(e2)
        case _ => somebs
      }
    }
    case Right(t) => f(a).map((b) => b :: t)
  })

  def sequence[A,E](a: List[Either[E,A]]): Either[E,List[A]] = traverse[Either[E,A], A, E](a)((a) => a)

  // 4.8
  sealed trait DidItWork[+E, +A] {
    def map[B](f: A => B): DidItWork[E, B]

    def flatMap[EE >: E, B](f: A => DidItWork[EE, B]): DidItWork[EE, B]

    def orElse[EE >: E, B >: A](b: DidItWork[EE, B]): DidItWork[EE, B]

    def map2[EE >: E, B, C](b: DidItWork[EE, B])(f: (A, B) => C): DidItWork[EE, C]
  }

  case class NoItDidNot[E](errors: Seq[E]) extends DidItWork[E, Nothing] {
    override def map[B](f: (Nothing) => B): DidItWork[E, B] = this

    override def map2[EE >: E, B, C](b: DidItWork[EE, B])(f: (Nothing, B) => C): DidItWork[EE, C] = b match {
      case NoItDidNot(e) => NoItDidNot(errors ++ e)
      case _ => this
    }

    override def flatMap[EE >: E, B](f: (Nothing) => DidItWork[EE, B]): DidItWork[EE, B] = this

    override def orElse[EE >: E, B >: Nothing](b: DidItWork[EE, B]): DidItWork[EE, B] = b match {
      case NoItDidNot(e) => NoItDidNot(e ++ errors)
      case _ => b
    }
  }

  case class YesItDid[+A](value: A) extends DidItWork[Nothing, A] {
    override def map[B](f: (A) => B): DidItWork[Nothing, B] = YesItDid(f(value))

    override def map2[EE >: Nothing, B, C](b: DidItWork[EE, B])(f: (A, B) => C): DidItWork[EE, C] = b match {
      case YesItDid(b) => YesItDid(f(value, b))
      case NoItDidNot(e) => NoItDidNot(e)
    }

    override def flatMap[EE >: Nothing, B](f: (A) => DidItWork[EE, B]): DidItWork[EE, B] = f(value)

    override def orElse[EE >: Nothing, B >: A](b: DidItWork[EE, B]): DidItWork[EE, B] = this
  }

  def traversediw[A, B,E](a: List[A])(f: A => DidItWork[E,B]): DidItWork[E,List[B]] =
    a.foldRight[DidItWork[E,List[B]]](YesItDid(Nil: List[B]))((a, somebs) => somebs match {
      case NoItDidNot(e) => {
        val e2 = f(a)
        e2 match {
          case NoItDidNot(e2) => NoItDidNot(e2 ++ e)
          case _ => somebs
        }
      }
      case YesItDid(t) => f(a).map((b) => b :: t)
    })

  def sequence[A,E](a: List[DidItWork[E,A]]): DidItWork[E,List[A]] = traversediw[DidItWork[E,A], A, E](a)((a) => a)

}
