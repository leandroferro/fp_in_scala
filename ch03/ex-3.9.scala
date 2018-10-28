
import fpinscala.datastructures._

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def length[A](as: List[A]): Int = foldRight(as, 0)((_,c) => c+1)

println(length(Nil))
println(length(List()))
println(length(List(1)))
println(length(List(1,2)))
println(length(List("a", "b", "c", "d")))
