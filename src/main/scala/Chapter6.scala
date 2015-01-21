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

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = r => {
    val (a, r1) = ra(r)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((a, b) => a -> b)

  def nonNegativeInt: Rand[Int] = rng => int(rng) match {
    case (Int.MinValue, out) => nonNegativeInt(out)
    case (negative, nxt) if negative < 0 => (-negative, nxt)
    case ok => ok
  }

  val double: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)

  val intDouble: Rand[(Int, Double)] = both(int, double)

  val doubleInt: Rand[(Double, Int)] = both(double, int)

  val double3: Rand[(Double, Double, Double)] = map2(map2(double, double)((_,_)),double)((a,b)=>(a._1,a._2,b))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(Nil:List[A]))((a:Rand[A],b:Rand[List[A]])=>map2(a,b)((a,b)=>a::b))

  val ints: Int => Rand[List[Int]] = count => sequence(List.fill(count)(int))
}
