def depth[A](tree: Tree[A]): Int =
  tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
