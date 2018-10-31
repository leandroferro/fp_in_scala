import fpinscala.datastructures._

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
  as match {
    case Nil => z
    case Cons(a, t) => foldLeft(t, f(z, a))(f)
  }

def append[A](a: A, as: List[A]): List[A] = 
  as match {
    case Nil => Cons(a, Nil)
    case Cons(x, xs) => Cons(x, append(a, xs))
  }

def appendFold[A](a: A, as: List[A]): List[A] = foldRight(as, Cons(a, Nil))((x, b) => Cons(x, b))
