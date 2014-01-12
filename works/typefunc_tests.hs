class Cool[T]
	type R

instance Cool bool
	struct R

struct Hello Type
	field Cool[Type].R

a2[T](i T)
	.hello Cool[T].R
	.var Hello[T]
	
export b2()
	a2(true)

		
a[T Cool]()
	.var1 T
	.var2 Cool[T].R
	var1 = var2
	
export b()
	a[bool]()

	

am[T, U](i T)
	.var Cool[T].R
	.var2 Cool[U].R
	var = (var2 = 1)
	
export b3()
	am[bool, bool](true)
