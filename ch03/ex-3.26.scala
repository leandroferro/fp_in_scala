def maximum(tree: Tree[Int]): Int = 
  tree match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }
