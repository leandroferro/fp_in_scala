def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
	@annotation.tailrec
	def go(pos: Int): Boolean = 
		if (pos >= as.length) true
		else if (ordered(as(pos-1), as(pos))) go(pos+1)
		else false

	go(1)
}
