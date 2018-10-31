import fpinscala.datastructures._

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
  as match {
    case Nil => z
    case Cons(a, t) => foldLeft(t, f(z, a))(f)
  }

def append[A](a: A, as: List[A]): List[A] = foldRight(as, Cons(a, Nil))((x, b) => Cons(x, b))

def concatenate[A](as: List[List[A]]): List[A] = foldLeft(as, Nil:List[A])((acc, l) => foldLeft(l, acc)((acc2, x) => append(x, acc2)))

def map[A, B](as: List[A])(f: A => B): List[B] = 
  as match {
    case Nil => Nil
    case Cons(a, t) => Cons(f(a), map(t)(f))
  }

def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concatenate(map(as)(f))

def filter[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if(f(a)) List(a) else Nil)

def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
  (l1, l2) match {
    case (Cons(a, t1), Cons(b, t2)) => Cons(f(a, b), zipWith(t1, t2)(f))
    case _ => Nil
  }
