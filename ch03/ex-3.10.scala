import fpinscala.datastructures._

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {

  def go(acc: B, t: List[A]): B = 
    t match {
      case Nil => acc
      case Cons(a, r) => go(f(acc, a), r)
    }

  go(z, as)
}

def print[A](as: List[A]): Unit = foldLeft(as, 0: Unit)((_, a) => println(a))

print(Nil)
print(List(1))
print(List("a", "b", "c"))
