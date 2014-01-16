class Cool[T]
	type R

instance Cool bool
	alias R = bool

struct Hello[Type]
	field Cool[Type].R

a2[T](i T)
	.hello Cool[T].R = undef()
	.var Hello[T] = undef()
	
export b2()
	a2(true)

		
a[T Cool]()
	.var1 T = undef()
	.var2 Cool[T].R = undef()
	var1 = var2
	
export b()
	a[bool]()

	

am[T, U](i T)
	.var Cool[T].R = undef()
	.var2 Cool[U].R = undef()
	var = (var2 = false)
	
export b3()
	am[bool, bool](true)
