def fold[A, B](tree: Tree[A])(z: A => B, c: (A, A) => B)
