def curry[A,B,C](f: (A,B) => C): A => (B => C) =
	(a:A) => (b:B) => f(a,b)

def sum(a:Int,b:Int) : Int  = a + b

def uncurry[A,B,C](f: A=>B=>C):(A,B)=>C =
	(a:A,b:B) => f(a)(b)

def currysum = curry(sum)

println(currysum(666)(333))

def add = uncurry(currysum)

println(add(666,333))
/*
don't compile as expected:
println(currysum(666,333))
println(sum(666)(333))
*/
