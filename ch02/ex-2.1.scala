def fib(n: Int): Int = {
	@annotation.tailrec
	def go(i: Int, prev: Int, curr: Int): Int =
		if (i >= n) curr
		else go(i+1, curr, curr + prev)

	go(1, 0, 1)
}
