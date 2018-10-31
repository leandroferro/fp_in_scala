import fpinscala.datastructures._

def map[A, B](as: List[A], f: A => B): List[B] = 
  as match {
    case Nil => Nil
    case Cons(a, t) => Cons(f(a), map(t, f))
  }
