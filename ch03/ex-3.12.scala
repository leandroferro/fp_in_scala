
import fpinscala.datastructures._

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {

  def go(acc: B, t: List[A]): B = 
    t match {
      case Nil => acc
      case Cons(a, r) => go(f(acc, a), r)
    }

  go(z, as)
}

def reverse[A](as: List[A]): List[A] = {

  def append(as: List[A], a: A): List[A] = 
    as match {
      case Nil => Cons(a, Nil)
      case Cons(x, Nil) => Cons(x, Cons(a, Nil))
      case Cons(x, t) => Cons(x, append(t, a))
    }

  as match {
    case Nil => Nil
    case Cons(a, t) => append(reverse(t), a)
  }
}

def reverse2[A](as: List[A]): List[A] = 
  as match {
    case Nil => Nil
    case Cons(a, t) => foldLeft(t, Cons(a, Nil))((nt, b) => Cons(b, nt))
  }
