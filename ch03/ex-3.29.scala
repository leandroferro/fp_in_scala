def fold[A, B](tree: Tree[A])(z: A => B, c: (B, B) => B): B = 
  tree match {
    case Leaf(v) => z(v)
    case Branch(l, r) => c(fold(l)(z, c), fold(r)(z, c))
  }

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
  fold[A, Tree[B]](tree)(v => Leaf(f(v)), (l, r) => Branch(l, r))

def depth[A](tree: Tree[A]): Int = 
  fold[A, Int](tree)(_ => 1, (l, r) => 1 + (l max r))

def maximum(tree: Tree[Int]): Int =
  fold[Int, Int](tree)(identity, (l, r) => l max r)

def size[A](tree: Tree[A]): Int =
  fold[A, Int](tree)(_ => 1, (l, r) => 1 + l + r)

