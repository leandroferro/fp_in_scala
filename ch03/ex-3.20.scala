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
