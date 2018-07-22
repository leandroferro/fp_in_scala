import fpinscala.datastructures._

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
	def go(ll: List[A]): List[A] = ll match {
		case Nil => Nil
		case Cons(h, t) if f(h) => go(t)
		case _ => ll
	}

	go(l)
}

println(dropWhile[Int](List(), x => false))

println(dropWhile[Int](List(1,2,3), x => false))

println(dropWhile[Int](List(1,2,3), x => (x % 2) == 0))

println(dropWhile[Int](List(2,4,5,6), x => (x % 2) == 0))

println(dropWhile[Int](List(1,1,1,2), x => x == 1))

println(dropWhile[Int](List(1,1,1,2), x => true))

println(dropWhile[Int](Nil, x => true))
