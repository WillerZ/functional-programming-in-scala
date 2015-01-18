import scala.annotation.tailrec

/**
 * Created by philwill on 17/01/15.
 */
object Chapter3 {
  //3.1 : Value 3, type Int

  //3.2
  def tail[A](l: List[A]): List[A] = l match {
    case _ :: t => t
    case _ => ???
  }

  //3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
    case _ :: t => h :: t
    case _ => ???
  }

  //3.4
  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n < 0) ???
    else if (n == 0) l
    else l match {
      case _ :: t => drop(t, n - 1)
      case _ => ???
    }
  }

  //3.5
  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case h :: t if f(h) => dropWhile(t, f)
      case x => x
    }
  }

  //3.6
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def initInternal(m: List[A], l: List[A]): List[A] =
      l match {
        case h :: Nil => m
        case h :: t => initInternal(m :+ h, t)
        case Nil => ???
      }
    initInternal(Nil, l)
  }

  // 3.7
  // No, because of JVM limitations. The function passed to foldRight cannot be invoked until all of its parameters are
  // ready, which means inspecting every element of the list (by the definition of foldRight).
  //
  // This would not be a correct shortcut to take as there are several values which do not have the result .0 when
  // multiplied by .0; for example Double.NaN, Double.PositiveInfinity and Double.NegativeInfinity. The only valid
  // shortcut is if you see a NaN: the result of a * b is Double.NaN for all a if b is Double.NaN and for all b if a is
  // Double.NaN.
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case h :: t => f(h, foldRight(t, z)(f))
      case Nil => z
    }
  }

  def product(as: List[Double]): Double = foldRight(as, 1.0)((a, b) => a * b)

  // 3.8
  // That invocation is an identity function operation for the combination of the List type and the foldRight method

  // 3.9
  def length[A](as: List[A]): Int = as.foldRight(0)((a: A, b: Int) => b + 1)

  // 3.10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case h :: t => foldLeft(t, f(z, h))(f)
    case Nil => z
  }

  // 3.11
  def sumLeft(ds: List[Double]): Double = foldLeft(ds, .0)((a: Double, b: Double) => a + b)

  def productLeft(ds: List[Double]): Double = foldLeft(ds, 1.0)((a: Double, b: Double) => a * b)

  def lengthLeft[A](ds: List[A]): Int = foldLeft(ds, 0)((a: Int, b: A) => a + 1)

  // 3.12
  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((t: List[A], h: A) => h :: t)

  // 3.13
  def foldLeftAsRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b: B) => b)((a: A, g: B => B) => (b: B) => g(f(b, a)))(z)
  }

  def foldRightAsLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft[A, B => B](as, (b: B) => b)((g: B => B, a: A) => (b: B) => g(f(a, b)))(z)
  }

  // 3.14
  def append[A](as: List[A], otheras: List[A]): List[A] = foldRightAsLeft(as, otheras)((a: A, b: List[A]) => {
    println(".");
    a :: b
  })

  // 3.15
  def concatenate[A](aas: List[List[A]]): List[A] = foldRightAsLeft[List[A], List[A]](aas, Nil: List[A])((as: List[A], bs: List[A]) => append(as, bs))

  // 3.16
  def incrementInts(as: List[Int]): List[Int] = foldRightAsLeft(as, Nil: List[Int])((a: Int, b: List[Int]) => a + 1 :: b)

  // 3.17
  def doublesToStrings(as: List[Double]): List[String] = foldRightAsLeft(as, Nil: List[String])((a: Double, b: List[String]) => a.toString :: b)

  // 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightAsLeft(as, Nil: List[B])((a: A, b: List[B]) => f(a) :: b)

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightAsLeft(as, Nil: List[A])((a: A, b: List[A]) => if (f(a)) a :: b else b)

  // 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRightAsLeft(as, Nil: List[B])((a: A, b: List[B]) => append(f(a), b))

  // 3.21
  def flatMapFilter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a: A) => if (f(a)) List(a) else Nil: List[A])

  // 3.22
  def addCorresponding(as: List[Int], bs: List[Int]): List[Int] = {
    as match {
      case ah :: at => bs match {
        case bh :: bt => ah + bh :: addCorresponding(at, bt)
        case _ => Nil: List[Int]
      }
      case _ => Nil: List[Int]
    }
  }

  def addCorrespondingAll(as: List[Int], bs: List[Int]): List[Int] = {
    as match {
      case ah :: at => bs match {
        case bh :: bt => ah + bh :: addCorrespondingAll(at, bt)
        case _ => as
      }
      case _ => bs
    }
  }

  // 3.23
  def zipWithObvious[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    as match {
      case ah :: at => bs match {
        case bh :: bt => f(ah, bh) :: zipWith(at, bt)(f)
        case _ => Nil: List[C]
      }
      case _ => Nil: List[C]
    }
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
    @tailrec
    def zipWith2(cs: List[C], as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
      as match {
        case ah :: at => bs match {
          case bh :: bt => zipWith2(f(ah, bh) :: cs, at, bt)(f)
          case _ => cs
        }
        case _ => cs
      }
    }
    reverse(zipWith2(Nil: List[C], as, bs)(f))
  }

  def zipWithAll[A, B, C](as: List[A], bs: List[B])(a: A, b: B)(f: (A, B) => C): List[C] = {
    @tailrec
    def zipWith2(cs: List[C], as: List[A], bs: List[B])(f: (A, B) => C): List[C] = {
      as match {
        case ah :: at => bs match {
          case bh :: bt => zipWith2(f(ah, bh) :: cs, at, bt)(f)
          case _ => zipWith2(f(ah, b) :: cs, at, Nil: List[B])(f)
        }
        case _ => bs match {
          case bh :: bt => zipWith2(f(a, bh) :: cs, Nil: List[A], bt)(f)
          case _ => cs
        }
      }
    }
    reverse(zipWith2(Nil: List[C], as, bs)(f))
  }

  // 3.24
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def hasHeadSequence[A](sup: List[A], sub: List[A]): Boolean = {
      sub match {
        case subh :: subt => sup match {
          case suph :: supt => (subh == suph) && hasHeadSequence(supt, subt)
          case _ => false
        }
        case _ => true
      }
    }
    sub match {
      case subh :: subt => sup match {
        case suph :: supt => ((subh == suph) && hasHeadSequence(supt, subt)) || hasSubsequence(supt, sub)
        case _ => false
      }
      case _ => true
    }
  }

  // Trees
  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // 3.25
  def size[A](tree: Tree[A]): Int = {
    @tailrec
    def sizeRec(sofar: Int, first: Tree[A], second: Tree[A]): Int = {
      first match {
        case Leaf(_) => second match {
          case Leaf(_) => sofar + 2
          case Branch(l, r) => sizeRec(sofar + 2, l, r)
        }
        case Branch(l, r) => sizeRec(sofar, l, Branch(r, second))
      }
    }
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => sizeRec(1, l, r)
    }
  }

  // 3.26
  def maximum(tree: Tree[Int]): Int = {
    @tailrec
    def leftMost[A](tree: Tree[A]): A = {
      tree match {
        case Leaf(a) => a
        case Branch(l, _) => leftMost(l)
      }
    }
    @tailrec
    def maximumRec(sofar: Int, first: Tree[Int], second: Tree[Int]): Int = {
      first match {
        case Leaf(g) => second match {
          case Leaf(r) => sofar.max(g).max(r)
          case Branch(l, r) => maximumRec(sofar.max(g), l, r)
        }
        case Branch(l, r) => maximumRec(sofar, l, Branch(r, second))
      }
    }
    tree match {
      case Leaf(a) => a
      case Branch(l, r) => maximumRec(leftMost(l), l, r)
    }
  }

  // 3.27 - I have not yet worked out a tail-recursive form of this
  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(a) => 1
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }
  }

  // 3.28 - I have not yet worked out a tail-recursive form of this
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // 3.29
  def foldLeft[A, B](tree: Tree[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def foldLeftRec(first: Tree[A], second: Tree[A], z: B)(f: (B, A) => B): B = {
      first match {
        case Leaf(ll) => second match {
          case Leaf(rl) => f(f(z, ll), rl)
          case Branch(l, r) => foldLeftRec(l, r, f(z, ll))(f)
        }
        case Branch(l, r) => foldLeftRec(l, Branch(r, second), z)(f)
      }
    }
    tree match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeftRec(l, r, z)(f)
    }
  }

  def foldRight[A, B](tree: Tree[A], z: B)(f: (A, B) => B): B = {
    @tailrec
    def foldRightRec(first: Tree[A], second: Tree[A], z: B)(f: (A, B) => B): B = {
      second match {
        case Leaf(rl) => first match {
          case Leaf(ll) => f(ll, f(rl, z))
          case Branch(l, r) => foldRightRec(l, r, f(rl, z))(f)
        }
        case Branch(l, r) => foldRightRec(Branch(l, first), r, z)(f)
      }
    }
    tree match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRightRec(l, r, z)(f)
    }
  }

  def asList[A](tree: Tree[A]): List[A] = foldRight(tree, Nil: List[A])((a, b) => a :: b)

  def asReverseList[A](tree: Tree[A]): List[A] = foldLeft(tree, Nil: List[A])((a, b) => b :: a)

  def fold[A, B](tree: Tree[A])(fleaf: A => B, fbranch: (Tree[B], Tree[B]) => Tree[B]): Tree[B] = {
    tree match {
      case Leaf(a) => Leaf(fleaf(a))
      case Branch(l, r) => fbranch(fold(l)(fleaf, fbranch), fold(r)(fleaf, fbranch))
    }
  }

  // Not as good as tailrec version above
  def size_fold[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)(a => 1, (a, b) => a match {
      case Leaf(l) => b match {
        case Leaf(r) => Leaf(l + r + 1)
        case _ => ???
      }
      case _ => ???
    }) match {
      case Leaf(size) => size
      case _ => ???
    }
  }

  // Not as good as tailrec version above
  def maximum_fold(tree: Tree[Int]): Int = {
    fold[Int, Int](tree)(a => a, (a, b) => a match {
      case Leaf(l) => b match {
        case Leaf(r) => Leaf(l.max(r))
        case _ => ???
      }
      case _ => ???
    }) match {
      case Leaf(max) => max
      case _ => ???
    }
  }

  def depth_fold[A](tree: Tree[A]): Int = {
    fold[A, Int](tree)(a => 1, (a, b) => a match {
      case Leaf(l) => b match {
        case Leaf(r) => Leaf(1 + l.max(r))
        case _ => ???
      }
      case _ => ???
    }) match {
      case Leaf(depth) => depth
      case _ => ???
    }
  }

  def map_fold[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    fold[A, B](tree)(f, (a, b) => Branch(a, b))
  }

}
