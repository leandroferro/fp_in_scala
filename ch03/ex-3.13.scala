
import fpinscala.datastructures._

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

def foldLeftViaRight[X, Y](xs: List[X], y: Y)(f: (Y, X) => Y): Y = {

  def zFunction(yy: Y): Y = yy

  def fFunction(x: X, yFunction: Y => Y): Y => Y = 
    k => yFunction(f(k, x))

  foldRight[X, Y => Y](xs, zFunction)(fFunction) (y)

}

def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B =
  as match {
    case Nil => z
    case Cons(a, t) => foldLeft(t, f(z, a))(f)
  }

def foldRightViaLeft[X, Y](xs: List[X], y: Y)(f: (X, Y) => Y): Y = {
  
  def zFunction(yy: Y): Y = yy

  def fFunction(yFunction: Y => Y, x: X): Y => Y =
    k => yFunction(f(x, k))

  foldLeft[X, Y => Y](xs, zFunction)(fFunction) (y)
}
