import fpinscala.datastructures._

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {

  def go(acc: B, t: List[A]): B = 
    t match {
      case Nil => acc
      case Cons(a, r) => go(f(acc, a), r)
    }

  go(z, as)
}

def sum(as: List[Int]): Int = foldLeft(as, 0)(_+_)

def product(as: List[Double]): Double = foldLeft(as, 1.0)(_*_)

def length[A](as: List[A]): Int = foldLeft(as, 0)((s, _) => s+1)
