import fpinscala.datastructures._

def transform(as: List[Double]): List[String] = 
  as match {
    case Nil => Nil
    case Cons(a, t) => Cons(a.toString, transform(t))
  }
