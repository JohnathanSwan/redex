def fib(n:Int):Int = {
	@annotation.tailrec
	def recur(a:Int,b:Int,n:Int):Int = 
		if (n > 0) recur(b,a+b,n-1)
		else a
	recur(0,1,n)
}

println(fib(0))
println(fib(1))
println(fib(2))
println(fib(3))
println(fib(4))
println(fib(5))
println(fib(6))
println(fib(7))
