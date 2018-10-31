import fpinscala.datastructures._

def filter[A](as: List[A], f: A => Boolean): List[A] = 
  as match {
    case Nil => Nil
    case Cons(a, t) => if (f(a)) Cons(a, filter(t, f)) else filter(t, f)
  }
