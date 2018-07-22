import fpinscala.datastructures._

def setHead[A](h: A, l: List[A]): List[A] = l match {
	case Nil => Nil
	case Cons(_, t) => Cons(h, t)
}

println(setHead(1, Nil))

println(setHead("A", List("a", "b")))

println(setHead(42, List(24)))
