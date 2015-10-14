@annotation.tailrec
def isSorted[A](as: Array[A], ordered: (A,A) => Boolean) : Boolean =
	if (as.length <= 1) true
	else ordered(as.head,as.tail.head) && isSorted(as.tail, ordered)

println(isSorted(Array(1,2,3,4,5), (a:Int,b:Int) => a < b)) 
println(isSorted(Array(1,2,1,3,4,5), (a:Int,b:Int) => a < b)) 
