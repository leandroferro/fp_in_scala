import fpinscala.datastructures._

def tail[A](xs: List[A]): List[A] = xs match {
	case Nil => Nil
	case Cons(_, t) => t
}

println(tail(List(1,2,3)))

println(tail(List()))

println(tail(List(1)))

println(tail(Nil))
