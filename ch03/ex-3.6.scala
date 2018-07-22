
import fpinscala.datastructures._

def init[A](l: List[A]): List[A] = {

	def go(ll: List[A]): List[A] = ll match {
		case Nil => Nil
		case Cons(_, Nil) => Nil
		case Cons(h, t) => Cons(h, go(t))
	}

	go(l)
}

println(init(Nil))
println(init(List()))
println(init(List(1)))
println(init(List(1,2)))
println(init(List(1,2,3,4,5,6)))
