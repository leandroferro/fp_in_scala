import fpinscala.datastructures._

def add1(as: List[Int]): List[Int] = 
  as match {
    case Nil => Nil
    case Cons(a, t) => Cons(a+1, add1(t))
  }
