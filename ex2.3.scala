def curry[A,B,C](f: (A,B) => C): A => (B => C) =
	(a:A) => (b:B) => f(a,b)

def sum(a:Int,b:Int) : Int  = a + b

def currysum = curry(sum)
def add3 = currysum(3)
def add99 = currysum(99)

println(add3(2))
println(add3(3))
println(add99(1))


