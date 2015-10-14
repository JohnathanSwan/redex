def compose[A,B,C](f:B=>C, g:A=>B):A=>C =
	(a:A) => f(g(a))

def reciprocal(i:Int) = 1.0/i
def triple(d:Double) = "three " + d + "s are " +  3 * d

def triprecip = compose(triple,reciprocal)

println(triprecip(2))
