struct Test[T]
	field T

	action create
		field = 0
		
	struct Nested[A]
		action create
		
	shared test()
		a[Nested]()
	
a[S[ST]]()
	.var S[int]
	return var

export main() -> int
	a[Test]()
	a[Test[bool].Nested]()
	Test[uint].test()
	
	return 0
