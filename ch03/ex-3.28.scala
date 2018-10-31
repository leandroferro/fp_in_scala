def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
  tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
