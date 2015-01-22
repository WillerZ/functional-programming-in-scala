import scala.annotation.tailrec

/**
 * Created by philwill on 21/01/15.
 */
object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(state: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newState = (state * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFL
      val nextRNG = SimpleRNG(newState)
      val n = (newState >>> 16).toInt
      (n, nextRNG)
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(a -> _)

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs.foldRight(unit[S, List[A]](Nil: List[A]))((a: State[S, A], b: State[S, List[A]]) => a.map2(b)((a, b) => a :: b))
  }

  case class State[S, +A](run: S => (A, S)) {
    def apply(s: S): (A, S) = run(s)

    // 6.10
    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => rb.flatMap(b => State.unit(f(a, b))))

    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      val (a, r1) = run(_)
      State(g(a).run)
    }

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A): Rand[A] = State.unit[RNG, A](a)

  def mapBasic[A, B](s: Rand[A])(f: A => B): Rand[B] = State[RNG, B]({
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  })

  // 6.6
  def map2Basic[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = State[RNG, C](r => {
    val (a, r1) = ra(r)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  })

  // 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State[RNG, B](rng => {
    val (a, r1) = f(rng)
    g(a)(r1)
  })

  val nonNegativeInt: Rand[Int] = flatMap(int)({
    case Int.MinValue => nonNegativeInt
    case negative if negative < 0 => State(nxt => (-negative, nxt))
    case ok => State(nxt => (ok, nxt))
  })

  // 6.8
  val nonNegativeLessThan: (Int => Rand[Int]) = peak => {
    val limit = (Int.MaxValue / peak) * peak
    flatMap(nonNegativeInt)({
      case tooBig if tooBig > limit => nonNegativeLessThan(peak)
      case ok => State(nxt => (ok % peak, nxt))
    })
  }

  // 6.9
  def map[S, A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((a, b) => a -> b)

  // 6.5
  val double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)

  val intDouble: Rand[(Int, Double)] = both(int, double)

  val doubleInt: Rand[(Double, Int)] = both(double, int)

  val double3: Rand[(Double, Double, Double)] = map2(map2(double, double)((_, _)), double)((a, b) => (a._1, a._2, b))

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil: List[A]))((a: Rand[A], b: Rand[List[A]]) => map2(a, b)((a, b) => a :: b))

  val ints: Int => Rand[List[Int]] = count => sequence(List.fill(count)(int))

  // 6.11
  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def apply(input: Input): Machine = if (candies < 1) this
    else input match {
      case Coin if locked => Machine(false, candies, coins + 1)
      case Turn if !locked => Machine(true, candies - 1, coins)
      case ignored => this
    }
  }

  def simulateMachine(is: List[Input]): State[Machine, (Int, Int)] = State(m => is.foldLeft(((0, 0), m))((l, i) => {
    val ((ca1, co1), s) = l
    s(i) match {
      case o => ((ca1 + o.candies - s.candies, co1 + o.coins - s.coins), o)
    }
  })
  )
}
