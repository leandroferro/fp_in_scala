import fpinscala.datastructures._

def drop[A](l: List[A], n: Int): List[A] = {
	def go(ll: List[A], i: Int): List[A] = 
		if (i > n) ll
		else ll match {
			case Nil => Nil
			case Cons(_, t) => go(t, i+1)
		}

	go(l, 1)
}

println(drop(List(), 0))

println(drop(List(1,2,3), 2))

println(drop(List(1), 2))

println(drop(Nil, 1))
